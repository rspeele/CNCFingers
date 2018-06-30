/// Generates instructions for cutting CNC finger joints with the board flat on the machine bed.
/// Joints are cut into the Y axis with the cutter moving along the X axis between each joint.
/// 0,0 has the end mill touched off the top of the board, with the front and left sides tangent to it.
module CNCFingers.FingerCode
open FSharp.Data.UnitSystems.SI.UnitSymbols

/// This just exists so we can have shorthand names for a bunch of relevant things about the job. Doesn't track state.
type private InstructionGenerator(job : JobParameters) =
    let tool = job.Tool
    let board = job.Board
    let finger = job.Finger
    let di = tool.Diameter
    let rad = di * 0.5
    let feed = tool.FeedRate
    let doc = tool.DepthOfCut
    let stepover = di * tool.StepOver
    /// How high to hover over the workpiece while doing rapid moves.
    let zClearance = 0.010<m>
    /// When we move the bit into a pocket, the distance from the cusp of the pocket curve to the front of the board
    /// must equal the board thickness. At 0 we already cut the tool diameter deep. So we need to subtract the diameter.
    let pocketYMax = board.Thickness - di

    /// Only hypothetical, doesn't include trimming for 
    let fingerWidth = board.Width / float finger.Count
    // Notice that this cut adds the side allowance. This BOTH makes the cut wider by the allowance, and
    // makes the next finger thinner by the allowance. Thus, even though we only add it once, fingers and cuts
    // made in this way will have the full allowance on both sides.
    let deltaXWithinPocket = fingerWidth + finger.SideAllowance - di

    // Within .01mm
    let (=~=) x y = abs (x - y) < 0.000_01<m>

    let scaleFeed (forDoc : float<m>) =
        if forDoc <= 0.0<m> then feed else
        let scale = doc / forDoc
        if scale < 1.0 then feed
        else feed * (min tool.MaxScaleFeedRate scale)

    // Cuts up and to the right right if direction is Clockwise, otherwise up and to the left.
    let curveArcInstruction direction (x : float<m>) =
        // We ignore the DOC setting here, which is _wrong_, but shouldn't be _disastrous_. This is because the
        // deepest we cut is equal to the tool radius, which is a pretty safe bet for DOC in wood anyway.
        let centerDeltaX =
            match direction with
            | CounterClockwise -> rad
            | Clockwise -> -rad
        let arc =
            {   Plane = XZ
                Direction = direction
                // Relative to the starting position of the cut.
                Center = centerDeltaX, 0.0<m>
            }
        Arc (feed, [ Z, -finger.EndAllowance; X, x + centerDeltaX ], arc)

    // Start at Z=0. Ends at starting X, Y=-rad, Z=0.
    let cutCurve direction (x : float<m>) =
        seq {
            // Check for shortcut, don't both creating a back-curve in thin air on the far left where we've just cut out
            // a pocket.
            if not finger.Multipass && direction = Clockwise && x =~= 0.0<m> then () else
            // Same on the far right.
            let farRight = board.Width - di
            if not finger.Multipass && direction = CounterClockwise && (x =~= farRight || x > farRight) then () else
            // Ok, now the main part.
            let maxY =
                if (x <= fingerWidth || x >= board.Width - fingerWidth) then
                    pocketYMax
                else
                    pocketYMax - rad
            let yPasses = [| -rad .. stepover .. maxY |]
            let mutable previousSlicePass = -1.0<m>
            for i in 0 .. yPasses.Length - 1 do
                let y = yPasses.[i]
                yield RapidMove [ X, x; Y, y ]
                yield RapidMove [ Z, -rad - finger.EndAllowance ]
                yield curveArcInstruction direction x
                // We don't need to do a trim pass every time, but we do need to do it when we are approaching the
                // tool diameter away from the last trim pass.
                if i = yPasses.Length - 1 || y - previousSlicePass >= di - stepover then
                    if direction = Clockwise then
                        // We are cutting a back-curve. Go ahead and trim off the top excess (end allowance)
                        // from the previous cut, too.
                        yield Move(scaleFeed finger.EndAllowance, [ X, x - fingerWidth ])
                        previousSlicePass <- y
                    elif not finger.Multipass && direction = CounterClockwise && (x > board.Width - fingerWidth * 2.0) then
                        // We are cutting the last finger on the right. Trim off the top excess since we won't do
                        // a back-curve and hit the above path to do it.
                        yield Move(scaleFeed finger.EndAllowance, [ X, x + fingerWidth ])
                        previousSlicePass <- y
            yield RapidMove [ X, x; Y, -rad ]
        }

    /// Assuming we're starting from Y=-di (tool just off the front face of the board).
    let cutPocketPass (x : float<m>) (z : float<m>) rate =
        seq {
            yield Move(rate, [ Y, pocketYMax ])
            if not finger.Multipass && x =~= 0.0<m> then
                // We are at the left edge of the board: eliminate the little |/ stickout with a quick back-and-forth
                yield Move(rate, [ X, -di ])
                yield RapidMove [ X, x ]
            if board.Thickness + z > finger.ShortcutThickness then
                // Clear the middle out. Wasteful in time but less wasteful than getting the tool caught on it and losing
                // a step.
                for i, xPass in seq { x + di .. di .. x + deltaXWithinPocket } |> Seq.indexed do
                    yield Move(feed, [ X, xPass ])
                    yield Move(feed, [ Y, if i % 2 = 0 then -rad else pocketYMax ])
                    if i % 2 <> 0 then
                        // Quick left-right move to knock out the nubbin left in the middle from going out and back
                        yield Move(feed, [ X, xPass - di ])
                        yield RapidMove [ X, xPass ]
            yield RapidMove [ Y, pocketYMax ]
            yield Move(feed, [ X, x + deltaXWithinPocket ])
            if not finger.Multipass && x + fingerWidth =~= board.Width then
                // We are the right edge of the board: eliminate the little \| stickout with a quick forth-and-back
                yield Move(rate, [ X, board.Width ])
                yield RapidMove [ X, x + deltaXWithinPocket ]
            yield Move(rate, [ Y, -di ])
        }

    /// Get the z positions for progressing downwards by DOC at a time, including both start and finish.
    let zPasses start finish =
        if finish > start then failwith "Nonsense, shouldn't make z-passes bottom to top"
        seq {
            let mutable previous = start
            for z in start .. -doc .. finish do
                yield z, feed
                previous <- z
            if previous <> finish then
                yield finish, scaleFeed (abs (finish - previous))
        }

    /// Repeatedly runs cutPocketPass stepping down the Z-axis.
    let cutPocket (x : float<m>) =
        seq {
            yield RapidMove [ Z, zClearance ]
            yield RapidMove [ X, x; Y, -di ]
            for z, rate in zPasses (-doc) (-board.Thickness - finger.SpoilDepth) do
                yield RapidMove [ Y, -di; X, x ]
                yield RapidMove [ Z, z ]
                yield! cutPocketPass x z rate

            // Now cut the curves out of each side.
            yield RapidMove [ Z, 0.0<m> ]
            // The "back" curve is clockwise since we look at the XZ plane "down" the Y axis i.e. towards negative,
            // just like we would look at the XY plane "down" the Z axis.
            yield! cutCurve Clockwise x
            yield! cutCurve CounterClockwise (x + deltaXWithinPocket)
        }

    /// Assuming we're starting from Y=-rad and Z=0. Cuts a pocket at the given X position then repeats up to the board
    /// width.
    let cutPockets (startX : float<m>) =
        // Displacement between the left side edge of each pocket. Allowances are not involved in this, as that would
        // change the fingers/distance, and end up with unaligned fingers at the far end of the board.
        let l2lDisplacement = fingerWidth * 2.0
        seq {
            for x in startX .. l2lDisplacement .. (board.Width - di) do
                yield! cutPocket x
        }

    /// Cut out a thin dado along the top of the board, trimming out the whole area where the XZ plane and XY plane
    /// curves meet, thus avoiding the need to do any complex shaping to make matching elements. This dado
    /// will be hidden from view when the joint is assembled. 
    let cutDado =
        seq {
            yield RapidMove [ Z, zClearance ]

            let distanceFromEdges = fingerWidth / 3.0

            let leftX = distanceFromEdges
            let rightX =
                // If we're doing the first of a multipass sequence, we need the groove all the way across to meet
                // the other groove.
                if finger.Multipass then
                    board.Width + distanceFromEdges - rad
                else
                    board.Width - distanceFromEdges - di
            // Move over to the starting position. Half a finger in X should be good to ensure the dado both doesn't
            // touch the edge of the board, and covers all the places with relevant geometry.
            // In Y, we want to go in just as far as the pockets did.
            yield RapidMove [ X, leftX; Y, pocketYMax ]
            // Take up the slack (z clearance) rapidly
            yield RapidMove [ Z, 0.0<m> ]

            // Now we need to cut to the depth past the curves atop the fingers, which will be equal to the tool radius
            // plus end allowance.
            let finalZ = -rad - finger.EndAllowance

            let rampDistance = tool.Diameter * tool.RampFactor

            for z, rate in zPasses (-doc) finalZ do
                yield Move(tool.PlungeRate, [ X, leftX + rampDistance; Z, z ]) // Ramp down to the right.
                yield Move(rate, [ X, rightX ]) // Cut the rest of the right straight across.
                yield RapidMove [ X, leftX + rampDistance ] // Go back to the ramp.
                yield Move(rate, [ X, leftX ]) // Clear the ramp.

            // In soft wood, you get fuzziness after doing this.
            // So run a quick couple verrrry thin passes over the edges.
            if finger.FuzzCut > 0.0<m> then
                let fuzzTrimSpeed = scaleFeed finger.FuzzCut
                yield RapidMove [ Z, -finger.FuzzCut ]
                yield Move(fuzzTrimSpeed, [ Y, pocketYMax + finger.FuzzCut ])
                yield Move(fuzzTrimSpeed, [ X, rightX ])
                yield Move(fuzzTrimSpeed, [ Y, pocketYMax - finger.FuzzCut ])
                yield Move(fuzzTrimSpeed, [ X, leftX ])
        }

    member this.Instructions() =
        seq {
            yield! cutDado
            yield RapidMove [ Z, zClearance ] // Get off the work
            yield RapidMove [ Y, -rad ] // Get in front of the work.

            match job.Start with
            | PocketThenFinger ->
                yield! cutPockets 0.0<m>
            | FingerThenPocket ->
                yield! cutPockets fingerWidth
            yield RapidMove [ Z, zClearance ]
        }

let instructions (job : JobParameters) =
    InstructionGenerator(job).Instructions()
    