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

    // Notice that this cut adds the side allowance. This BOTH makes the cut wider by the allowance, and
    // makes the next finger thinner by the allowance. Thus, even though we only add it once, fingers and cuts
    // made in this way will have the full allowance on both sides.
    let deltaXWithinPocket = finger.FingerWidth + finger.SideAllowance - di

                                                                                                                                 // TODO vertical allowance
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
        Arc (feed, [ Z, 0.0<m>; X, x + centerDeltaX ], arc)

    // Start at Z=0. Ends at starting X, Y=-rad, Z=0.
    let cutCurve direction (x : float<m>) =
        seq {
            for y in -rad .. stepover .. board.Thickness - rad do
                yield RapidMove [ X, x; Y, y ]
                yield RapidMove [ Z, -rad ]
                yield curveArcInstruction direction x
            yield RapidMove [ X, x; Y, -rad ]
        }

    /// Assuming we're starting from Y=-rad (tool just off the front face of the board).
    let cutPocketPass (x : float<m>) =
        [|  Move(feed, [ Y, pocketYMax ])
            Move(feed, [ X, x + deltaXWithinPocket ])
            Move(feed, [ Y, -rad ])
        |]

    /// Get the z positions for progressing downwards by DOC at a time, including both start and finish.
    let zPasses start finish =
        if finish > start then failwith "Nonsense, shouldn't make z-passes bottom to top"
        seq {
            yield! seq { start .. -doc .. finish }
            let idealPasses = abs (finish - start) / doc
            if 0.0001 < abs (idealPasses - round idealPasses) then // throw in a final pass if we weren't evenly divisible
                yield finish
        }

    /// Assuming we're starting from Y=-rad and Z=0. Repeatedly runs cutPocketPass stepping down the Z-axis.
    let cutPocket (x : float<m>) =
        seq {
            yield RapidMove [ Z, 0.0<m> ]
            for z in zPasses (-doc) (-board.Thickness) do
                yield RapidMove [ Y, -rad; X, x ]
                yield RapidMove [ Z, z ]
                yield! cutPocketPass x

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
        let l2lDisplacement = finger.FingerWidth * 2.0
        seq {
            for x in startX .. l2lDisplacement .. board.Width do
                yield! cutPocket x
        }

    /// Cut out a thin dado along the top of the board, trimming out the whole area where the XZ plane and XY plane
    /// curves meet, thus avoiding the need to do any complex shaping to make matching elements. This dado
    /// will be hidden from view when the joint is assembled. 
    let cutDado =
        seq {
            yield RapidMove [ Z, zClearance ]

            let distanceFromEdges = finger.FingerWidth / 3.0

            let leftX = distanceFromEdges
            let rightX = board.Width - distanceFromEdges - di
            // Move over to the starting position. Half a finger in X should be good to ensure the dado both doesn't
            // touch the edge of the board, and covers all the places with relevant geometry.
            // In Y, we want to go in just as far as the pockets did.
            yield RapidMove [ X, leftX; Y, pocketYMax ]

            // Now we need to cut to the depth past the curves atop the fingers, which will be equal to the tool radius
            // plus end allowance.
            let finalZ = rad + finger.EndAllowance

            let rampDistance = tool.Diameter * tool.RampFactor

            for z in zPasses (-doc) finalZ do
                yield Move(tool.PlungeRate, [ X, leftX + rampDistance; Z, z ]) // Ramp down to the right.
                yield Move(feed, [ X, rightX ]) // Cut the rest of the right straight across.
                yield RapidMove [ X, leftX + rampDistance ] // Go back to the ramp.
                yield Move(feed, [ X, leftX ]) // Clear the ramp.
        }

    member this.Instructions() =
        seq {
            yield RapidMove [ Y, -rad; Z, zClearance ] // Get off the work, in front of the face.
            yield! cutPockets 0.0<m> // TODO: start at a different spot depending on job setup.
            yield! cutDado
        }

let instructions (job : JobParameters) =
    InstructionGenerator(job).Instructions()
    