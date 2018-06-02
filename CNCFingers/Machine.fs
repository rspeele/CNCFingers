namespace CNCFingers
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<Measure>] type machineunit

type MachineUnitMode =
    | Millimeters
    | Inches
    member this.Scale =
        match this with
        | Millimeters -> 1000.0<machineunit/m>
        | Inches -> 39.3701<machineunit/m>

type Machine =
    {   Unit : MachineUnitMode
    }
    static member Default =
        {   Unit = Millimeters
        }