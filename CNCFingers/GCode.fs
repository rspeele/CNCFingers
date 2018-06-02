namespace CNCFingers
open FSharp.Data.UnitSystems.SI.UnitSymbols

type Axis =
    | X
    | Y
    | Z
    override this.ToString() =
        match this with
        | X -> "X"
        | Y -> "Y"
        | Z -> "Z"

type Plane =
    | XY
    | XZ
    | YZ
    override this.ToString() =
        match this with
        | XY -> "XY"
        | XZ -> "XZ"
        | YZ -> "YZ"

type ArcDirection =
    | Clockwise
    | CounterClockwise
    override this.ToString() =
        match this with
        | Clockwise -> "CW"
        | CounterClockwise -> "CCW"

type ArcParameters =
    {   Plane : Plane
        Direction : ArcDirection
        /// On axes of the selected plan. Relative to current position.
        Center : float<m> * float<m>
    }

type Instruction =
    | RapidMove of (Axis * float<m>) list
    | Move of float<m/s> * (Axis * float<m>) list
    | Arc of float<m/s> * (Axis * float<m>) list * ArcParameters
    member this.ToGCode(machine : Machine) =
        let inline feedrate (f : float<machineunit/s>) =
            let perMinute : float = f / 1.0<machineunit/s> * 60.0
            "F" + string perMinute
        let inline mac (f : float<machineunit>) =
            string f
        let inline xyzs (axes : (Axis * float<m>) list) =
            seq {
                for (axis, meters) in axes -> string axis + mac (meters * machine.Unit.Scale)
            } |> String.concat " "
        match this with
        | RapidMove axes -> "G0 " + xyzs axes
        | Move (speed, axes) -> "G1 " + feedrate (speed * machine.Unit.Scale) + " " + xyzs axes
        | Arc (speed, axes, path) ->
            let planeSwitch =
                match path.Plane with
                | XY -> "G17"
                | XZ -> "G18"
                | YZ -> "G19"
            let moveCommand =
                match path.Direction with
                | Clockwise -> "G2"
                | CounterClockwise -> "G3"
            let f = feedrate (speed * machine.Unit.Scale)
            let endPoint = xyzs axes
            let ijkX, ijkY =
                match path.Plane with
                | XY -> "I", "J"
                | XZ -> "I", "K"
                | YZ -> "J", "K"
            let centerPoint =
                let x, y = path.Center
                ijkX + mac (x * machine.Unit.Scale) + " " + ijkY + mac (y * machine.Unit.Scale)
            [|  planeSwitch
                moveCommand
                f
                endPoint
                centerPoint
            |] |> String.concat " "

    override this.ToString() = this.ToGCode(Machine.Default)
    