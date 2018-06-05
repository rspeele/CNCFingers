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

    // Notice that this cut adds the side allowance. This BOTH makes the cut wider by the allowance, and
    // makes the next finger thinner by the allowance. Thus, even though we only add it once, fingers and cuts
    // made in this way will have the full allowance on both sides.
    let deltaXWithinPocket = finger.FingerWidth + finger.SideAllowance - di

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
        [|  Move(feed, [ Y, board.Thickness - rad ])
            Move(feed, [ X, x + deltaXWithinPocket ])
            Move(feed, [ Y, -rad ])
        |]

    /// Assuming we're starting from Y=-rad and Z=0. Repeatedly runs cutPocketPass stepping down the Z-axis.
    let cutPocket (x : float<m>) =
        seq {
            yield RapidMove [ Z, 0.0<m> ]
            for z in -doc .. -doc .. -board.Thickness do
                yield RapidMove [ Y, -rad; X, x ]
                yield RapidMove [ Z, z ]
                yield! cutPocketPass x
            // The above loop won't include the final pass unless it happened to be an exact multiple of the DOC.
            // So check for that.
            let idealNumPasses = board.Thickness / doc
            if 0.001 < abs (idealNumPasses - round idealNumPasses) then
                yield RapidMove [ Z, -board.Thickness ]
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

    member this.Instructions() =
        seq {
            yield RapidMove [ Y, -rad ] // Get off the work, in front of the face.
            yield! cutPockets 0.0<m> // TODO: start at a different spot depending on job setup.

        }

let instructions (job : JobParameters) =
    InstructionGenerator(job).Instructions()
    