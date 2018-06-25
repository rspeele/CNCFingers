module CNCFingers.GCodeTransform
open FSharp.Data.UnitSystems.SI.UnitSymbols

/// Rotation 90 degrees clockwise about the Z axis.
let clockwise90 =
    let translateMove ((axis, scalar) as pair) =
        match axis with
        | X -> Y, -scalar
        | Y -> X, scalar
        | Z -> pair
    function
    | RapidMove moves -> RapidMove [ for move in moves -> translateMove move ]
    | Move (speed, moves) -> Move (speed, [ for move in moves -> translateMove move ])
    | Arc (speed, moves, pars) ->
        let newMoves = [ for move in moves -> translateMove move ]
        let newPars =
            {   Plane =
                    match pars.Plane with
                    | XY -> XY // XY plane stays as-is.
                    | XZ -> YZ
                    | YZ -> XZ
                Direction =
                    match pars.Plane, pars.Direction with
                    // the XY plane stays as-is, XZ, going from looking down Y to looking down X also preserves arc direction.
                    | (XY | XZ), any -> any
                    // when we go from looking down X to looking *up* Y, we have to reverse arc directions
                    | YZ, Clockwise -> CounterClockwise
                    | YZ, CounterClockwise -> Clockwise
                Center =
                    let d1, d2 = pars.Center
                    match pars.Plane with
                    | XY -> d2, -d1 // as in translateMove
                    | XZ -> -d1, d2
                    | YZ -> pars.Center
            }
        Arc (speed, newMoves, newPars)

let clockwise180 = clockwise90 >> clockwise90
let clockwise270 = clockwise180 >> clockwise90

let mirrorX =
    let translateMove ((axis, scalar) as pair) =
        match axis with
        | X -> X, -scalar
        | Y | Z -> pair
    function
    | RapidMove moves -> RapidMove [ for move in moves -> translateMove move ]
    | Move (speed, moves) -> Move (speed, [ for move in moves -> translateMove move ])
    | Arc (speed, moves, pars) ->
        let newMoves = [ for move in moves -> translateMove move ]
        let newPars =
            {   Plane = pars.Plane
                Direction =
                    match pars.Plane, pars.Direction with
                    | XY, Clockwise -> CounterClockwise
                    | XY, CounterClockwise -> Clockwise
                    | XZ, Clockwise -> CounterClockwise
                    | XZ, CounterClockwise -> Clockwise
                    | YZ, any -> any
                Center =
                    let d1, d2 = pars.Center
                    match pars.Plane with
                    | XY | XZ -> -d1, d2
                    | YZ -> pars.Center
            }
        Arc (speed, newMoves, newPars)

let mirrorY =
    let translateMove ((axis, scalar) as pair) =
        match axis with
        | Y -> Y, -scalar
        | X | Z -> pair
    function
    | RapidMove moves -> RapidMove [ for move in moves -> translateMove move ]
    | Move (speed, moves) -> Move (speed, [ for move in moves -> translateMove move ])
    | Arc (speed, moves, pars) ->
        let newMoves = [ for move in moves -> translateMove move ]
        let newPars =
            {   Plane = pars.Plane
                Direction =
                    match pars.Plane, pars.Direction with
                    | XY, Clockwise -> CounterClockwise
                    | XY, CounterClockwise -> Clockwise
                    | YZ, Clockwise -> CounterClockwise
                    | YZ, CounterClockwise -> Clockwise
                    | XZ, any -> any
                Center =
                    let d1, d2 = pars.Center
                    match pars.Plane with
                    | XY -> d1, -d2
                    | YZ -> -d1, d2
                    | XZ -> pars.Center
            }
        Arc (speed, newMoves, newPars)

let translate (dx, dy, dz) =
    let translateMove (axis, scalar) =
        axis, scalar +
            match axis with
            | X -> dx
            | Y -> dy
            | Z -> dz
    function
    | RapidMove moves -> RapidMove [ for move in moves -> translateMove move ]
    | Move (speed, moves) -> Move (speed, [ for move in moves -> translateMove move ])
    | Arc (speed, moves, pars) ->
        let newMoves = [ for move in moves -> translateMove move ]
        // since the center is relative, there is no need to translate it. direction and plane stay the same too.
        Arc (speed, newMoves, pars)

let () =
    let instructions =
        [   RapidMove [ X, 1.0<m>; Y, 1.0<m>; Z, 1.0<m> ]
            Move (100.0<m/s>, [ X, -1.0<m>; Y, -1.0<m>; Z, -1.0<m> ])
            Arc (50.0<m/s>, [ X, 1.0<m>; Y, 1.0<m>; Z, 1.0<m> ], { Plane = XY; Direction = Clockwise; Center = 5.0<m>, -5.0<m> })
            Arc (40.0<m/s>, [ X, 1.0<m>; Y, 1.0<m>; Z, 1.0<m> ], { Plane = XZ; Direction = Clockwise; Center = 7.0<m>, -7.0<m> })
            Arc (30.0<m/s>, [ X, 1.0<m>; Y, 1.0<m>; Z, 1.0<m> ], { Plane = YZ; Direction = Clockwise; Center = 2.0<m>, -2.0<m> })
        ]
    let clockwise360 = clockwise270 >> clockwise90
    let spun = List.map clockwise360 instructions
    if spun <> instructions then
        failwith "360 clockwise rotation is not identity -- bug"

    let flipfloppedX = List.map (mirrorX >> mirrorX) instructions
    if flipfloppedX <> instructions then
        failwith "Double x-flip is not identity -- bug"

    let flipfloppedY = List.map (mirrorY >> mirrorY) instructions
    if flipfloppedY <> instructions then
        failwith "Double y-flip is not identity -- bug"

    let translatedOutAndBack = List.map (translate(1.0<m>, -2.0<m>, 3.0<m>) >> translate(-1.0<m>, 2.0<m>, -3.0<m>)) instructions
    if translatedOutAndBack <> instructions then
        failwith "Translate and back is not identity -- bug"

