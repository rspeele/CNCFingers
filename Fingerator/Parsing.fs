module Fingerator.Parsing
open FSharp.Data.UnitSystems.SI.UnitSymbols
open CNCFingers
open FParsec

module private Guts =
    type DimensionalTransform = float -> float<m>
    type TimeTransform = float -> float<s>
    type Parser<'x> = Parser<'x, unit>

    module Units =
        let inch = 0.0254<m>
        let thou = inch * 0.001
        let minute = 60.0<s>
        let mm = 0.001<m>
        let cm = 0.01<m>

    let inches : Parser<DimensionalTransform> =
        choice
            [   pstringCI "inches"
                pstringCI "inch"
                pstringCI "in"
                pstringCI "\""
                pstringCI "freedom units"
            ]
        >>% (*) Units.inch

    let thousandths : Parser<DimensionalTransform> =
        choice
            [   pstringCI "thousandth" .>> optional (pstringCI "s")
                pstringCI "thou"
            ]
        >>% (*) Units.thou

    let millimeters : Parser<DimensionalTransform> =
        choice
            [   pstringCI "millimeter" .>> optional (pstringCI "s")
                pstringCI "millimetre" .>> optional (pstringCI "s")
                pstringCI "mm"
            ]
        >>% (*) Units.mm

    let centimeters : Parser<DimensionalTransform> =
        choice
            [   pstringCI "centimeter" .>> optional (pstringCI "s")
                pstringCI "centimetre" .>> optional (pstringCI "s")
                pstringCI "cm"
            ]
        >>% (*) Units.cm

    let meters : Parser<DimensionalTransform> =
        choice
            [   pstringCI "meter" .>> optional (pstringCI "s")
                pstringCI "metre" .>> optional (pstringCI "s")
                pstringCI "m"
            ]
        >>% (*) 1.0<m>

    let dimensionalTransform =
        choice
            [   inches
                thousandths
                millimeters
                centimeters
                meters
            ]

    let seconds : Parser<TimeTransform> =
        choice
            [   pstringCI "second" .>> optional (pstringCI "s")
                pstringCI "s"
            ]
        >>% (*) 1.0<s>

    let minutes : Parser<TimeTransform> =
        choice
            [   pstringCI "minute" .>> optional (pstringCI "s")
                pstringCI "min" .>> optional (pstringCI "s")
            ]
        >>% (*) Units.minute

    let timeTransform =
        choice
            [   seconds
                minutes
            ]

    let ws = spaces
    let ws1 = spaces1
    let number = pfloat

    let distance =
        pipe2
            (number .>> ws)
            dimensionalTransform
            (|>)

    let speed =
        pipe2
            (distance .>> ws)
            (pchar '/' >>. ws >>. timeTransform)
            (fun distance time -> distance / time 1.0)

    type ConfigEdit = JobParameters -> JobParameters

    let distanceEdit name edit =
        pstringCI name >>. ws >>. pchar '=' >>. ws >>. distance |>> edit

    let toolDiameter : Parser<ConfigEdit> =
        distanceEdit "tool.diameter" (fun d job -> { job with Tool = { job.Tool with Diameter = d } })