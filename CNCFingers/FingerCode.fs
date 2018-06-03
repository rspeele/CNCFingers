/// Generates instructions for cutting CNC finger joints with the board flat on the machine bed.
/// Joints are cut into the Y axis with the cutter moving along the X axis between each joint.
/// 0,0 has the end mill touched off the top of the board, with the front and left sides tangent to it.
module private CNCFingers.FingerCode
open FSharp.Data.UnitSystems.SI.UnitSymbols

/// This just exists so we can have shorthand names for a bunch of relevant things about the job. Doesn't track state.
type InstructionGenerator(job : JobParameters) =
    let tool = job.Tool
    let board = job.Board
    let finger = job.Finger
    let di = tool.Diameter
    let rad = di * 0.5
    let feed = tool.FeedRate
    let doc = tool.DepthOfCut

    // Notice that this cut adds the side allowance. This BOTH makes the cut wider by the allowance, and
    // makes the next finger thinner by the allowance. Thus, even though we only add it once, fingers and cuts
    // made in this way will have the full allowance on both sides.
    let deltaXWithinPocket = finger.FingerWidth + finger.SideAllowance - di

    /// Assuming we're starting from Y=-rad (tool just off the front face of the board).
    let cutPocketPass (x : float<m>) (z : float<m>) =
        [|  Move(feed, [ Y, board.Thickness - rad ])
            Move(feed, [ X, x + deltaXWithinPocket ])
            Move(feed, [ Y, -rad ])
        |]

    /// Assuming we're starting from Y=-rad and Z=0. Repeatedly runs cutPocketPass stepping down the Z-axis.
    let cutPocket (x : float<m>) =
        seq {
            for z in -doc .. -doc .. -board.Thickness do
                yield! cutPocketPass x z
            // The above loop won't include the final pass unless it happened to be an exact multiple of the DOC.
            // So check for that.
            let idealNumPasses = board.Thickness / doc
            if 0.001 < abs (idealNumPasses - round idealNumPasses) then
                yield! cutPocketPass x -board.Thickness
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
        failwith ""

/// Assuming we're starting from Z = 0, Y = -job.Tool.Diameter
let cutPocket (job : JobParameters) (x : float<m>) =
    [   Move(job.Tool.FeedRate, [ X, x ])
    ]

let instructions (job : JobParameters) =
    seq {
        // Move off the front of the board.
        yield RapidMove [ Y, job.Tool.Diameter / 2.0 ]
    }
    