open System
open System.IO
open CNCFingers
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<EntryPoint>]
let main argv =
    let parameters = 
        let diameter = 0.00635<m>
        let boardWidth = 0.3<m>
        {   Tool =
                {   Diameter = diameter
                    StepOver = 0.1
                    DepthOfCut = diameter * 0.5
                    FeedRate = 0.025<m/s>
                    PlungeRate = 0.0125<m/s>
                    RampFactor = 2.0
                }
            Board =
                {   Width = boardWidth
                    Thickness = 0.0254<m>
                }
            Finger =
                {   FingerWidth = boardWidth / 20.0
                    SideAllowance = 0.0001<m>
                    EndAllowance = 0.0001<m>
                }
            Start = FingerThenPocket
        }
    let machine =
        {   Unit = Millimeters
        }
    let instructions = FingerCode.instructions parameters
    use output = new StreamWriter(File.Create("fingers.gcode"))
    for instruction in instructions do
        let gcode = instruction.ToGCode(machine)
        output.WriteLine(gcode)
    0 // return an integer exit code
