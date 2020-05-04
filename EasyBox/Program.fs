module EasyBox.Program
open System
open System.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols
open CNCFingers

let inch = 0.0254<m>
let ipm = inch / 60.0<s>

let config =
    {   Tool =
            {   Diameter = 0.125 * inch
                StepOver = 0.2
                DepthOfCut = 0.05 * inch
                FeedRate = 50.0 * ipm
                PlungeRate = 20.0 * ipm
                RampFactor = 2.0
                MaxScaleFeedRate = 1.25
            }
        Machine = { Unit = Millimeters }
        BoxType =
            {   Top =
                    {   LidThickness = 0.25 * inch
                        SlotClearance = 0.01 * inch
                    } |> fun x -> SlidingLid(x, Some TLatch) |> Some
                BottomCaptive =
                    {   LidThickness = 0.25 * inch
                        SlotClearance = 0.010 * inch
                    } |> Some
            }


        SideThickness = 0.25 * inch
        ExteriorDimensions = 2.375 * inch, 4.25 * inch, 1.5 * inch
        WoodExpansionFactor = 1.005
        ForceFingerCount = None
    }

let save (machine : Machine) (fileName : string)(instructions : Instruction seq) =
    use output = new StreamWriter(File.Create(fileName))
    for instruction in instructions do
        let gcode = instruction.ToGCode(machine)
        output.WriteLine(gcode)

[<EntryPoint>]
let main argv =
    let generator = BoxCode.BoxGenerator(config)
    generator.BackSide() |> save config.Machine "back.gcode"
    generator.LeftSide() |> save config.Machine "left.gcode"
    generator.RightSide() |> save config.Machine "right.gcode"
    generator.FrontSide() |> save config.Machine "front.gcode"
    generator.Top() |> save config.Machine "top.gcode"
    generator.Bottom() |> save config.Machine "bottom.gcode"
    0 // return an integer exit code
