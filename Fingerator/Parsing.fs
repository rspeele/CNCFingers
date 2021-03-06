﻿module Fingerator.Parsing
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

    let machineUnitMode : Parser<MachineUnitMode> =
        choice
            [   millimeters >>% Millimeters
                inches >>% Inches
            ]

    /// A line comment begins with -- and continues through the end of the line.
    let private lineComment =
        pstring "--" >>. restOfLine true |>> ignore

    /// A block comment begins with /* and continues until a trailing */ is found.
    /// Nested block comments are not allowed, so additional /* tokens found
    /// after the first are ignored.
    let private blockComment =
        pstring "/*" >>. skipCharsTillString "*/" true System.Int32.MaxValue

    /// Where whitespace is expected, it can be one of...
    let private whitespaceUnit =
        choice
            [   lineComment // a line comment
                blockComment // a block comment
                spaces1 // one or more whitespace characters
            ] <?> "whitespace"

    /// Optional whitespace: 0 or more whitespace units
    let ws = skipMany whitespaceUnit

    /// Required whitespace: 1 or more whitespace units
    let ws1 = skipMany1 whitespaceUnit

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

    let sp = skipMany (skipChar ' ')

    // Post-processing options
    let postProcessStep : Parser<Instruction -> Instruction> =
        let translate =
            pstringCI "translate"
            >>. sp
            >>. pchar '('
            >>. sp
            >>.
            pipe3
                (distance .>> sp .>> pchar ',' .>> sp)
                (distance .>> sp .>> pchar ',' .>> sp)
                (distance .>> sp .>> pchar ')' .>> sp)
                (fun x y z -> GCodeTransform.translate(x, y, z))
            
        choice
            [   pstringCI "identity" .>> sp >>% id
                pstringCI "clockwise90" .>> sp >>% GCodeTransform.clockwise90
                pstringCI "clockwise180" .>> sp >>% GCodeTransform.clockwise180
                pstringCI "clockwise270" .>> sp >>% GCodeTransform.clockwise270
                pstringCI "mirrorX" .>> sp >>% GCodeTransform.mirrorX
                pstringCI "mirrorY" .>> sp >>% GCodeTransform.mirrorY
                translate
            ]

    let postProcessPipeline =
        sepEndBy1 postProcessStep (pstring ">>" >>. sp)
        |>> List.fold (>>) id

    let copies =
        pchar '['
        .>> sp
        >>. sepEndBy1 postProcessPipeline (pchar ',' >>. sp)
        .>> sp
        .>> pchar ']'
        .>> sp


    let boolean =
        choice
            [   choice [ pstringCI "yes"; pstringCI "true" ] >>% true
                choice [ pstringCI "no"; pstringCI "false" ] >>% false
            ]

    let startWith =
        choice
            [   pstringCI "fingerthenpocket" >>% FingerThenPocket
                pstringCI "pocketthenfinger" >>% PocketThenFinger
            ]

    type ConfigEdit = JobParameters -> JobParameters

    let someEdit parser name edit =
        pstringCI name >>. ws >>. pchar '=' >>. ws >>. parser |>> edit

    let scaleEdit = someEdit number
    let distanceEdit = someEdit distance
    let speedEdit = someEdit speed

    let jobStartWith : Parser<ConfigEdit> =
        someEdit startWith "job.startwith" (fun start job -> { job with Start = start })
        
    let jobCopies : Parser<ConfigEdit> =
        someEdit copies "job.copies"
            (fun xform job -> { job with Copies = xform })

    let jobSingleFile : Parser<ConfigEdit> =
        someEdit boolean "job.singlefile"
            (fun single job -> { job with SingleFile = single })

    let toolDiameter : Parser<ConfigEdit> =
        distanceEdit "tool.diameter" (fun d job -> { job with Tool = { job.Tool with Diameter = d } })

    let toolStepOver : Parser<ConfigEdit> =
        scaleEdit "tool.stepover" (fun s job -> { job with Tool = { job.Tool with StepOver = s } })

    let toolDepthOfCut : Parser<ConfigEdit> =
        distanceEdit "tool.depthofcut" (fun doc job -> { job with Tool = { job.Tool with DepthOfCut = doc } })

    let toolFeedRate : Parser<ConfigEdit> =
        speedEdit "tool.feedrate" (fun feed job -> { job with Tool = { job.Tool with FeedRate = feed } })

    let toolPlungeRate : Parser<ConfigEdit> =
        speedEdit "tool.plungerate" (fun plunge job -> { job with Tool = { job.Tool with PlungeRate = plunge } })

    let toolRampFactor : Parser<ConfigEdit> =
        scaleEdit "tool.rampfactor" (fun ramp job -> { job with Tool = { job.Tool with RampFactor = ramp } })

    let toolScaleFeedRate : Parser<ConfigEdit> =
        scaleEdit "tool.maxscalefeedrate" (fun scale job -> { job with Tool = { job.Tool with MaxScaleFeedRate = scale } })

    let boardWidth : Parser<ConfigEdit> =
        distanceEdit "board.width" (fun width job -> { job with Board = { job.Board with Width = width } })

    let boardThickness : Parser<ConfigEdit> =
        distanceEdit "board.thickness" (fun thick job -> { job with Board = { job.Board with Thickness = thick} })

    let fingerCount : Parser<ConfigEdit> =
        someEdit pint32 "finger.count" (fun count job -> { job with Finger = { job.Finger with Count = count } })

    let fingerSideAllowance : Parser<ConfigEdit> =
        distanceEdit "finger.sideallowance"
            (fun allow job -> { job with Finger = { job.Finger with SideAllowance = allow } })

    let fingerEndAllowance : Parser<ConfigEdit> =
        distanceEdit "finger.endallowance"
            (fun allow job -> { job with Finger = { job.Finger with EndAllowance = allow } })

    let fingerSpoilDepth : Parser<ConfigEdit> =
        distanceEdit "finger.spoildepth"
            (fun depth job -> { job with Finger = { job.Finger with SpoilDepth = depth } })

    let fingerFuzz : Parser<ConfigEdit> =
        distanceEdit "finger.fuzzcut"
            (fun fuzz job -> { job with Finger = { job.Finger with FuzzCut = fuzz } })

    let fingerMulti : Parser<ConfigEdit> =
        someEdit boolean "finger.multipass"
            (fun multipass job -> { job with Finger = { job.Finger with Multipass = multipass } })

    let fingerShortcut : Parser<ConfigEdit> =
        distanceEdit "finger.shortcut"
            (fun shortcut job -> { job with Finger = { job.Finger with ShortcutThickness = shortcut } })

    let machineUnit : Parser<ConfigEdit> =
        someEdit machineUnitMode "machine.unit"
            (fun mode job -> { job with Machine = { job.Machine with Unit = mode } })

    let configEdit =
        choice
            [   jobStartWith
                jobCopies
                jobSingleFile
                toolDiameter
                toolStepOver
                toolDepthOfCut
                toolFeedRate
                toolPlungeRate
                toolRampFactor
                toolScaleFeedRate
                boardWidth
                boardThickness
                fingerCount
                fingerSideAllowance
                fingerEndAllowance
                fingerSpoilDepth
                fingerFuzz
                fingerMulti
                fingerShortcut
                machineUnit
            ]

    let jobParameters =
        ws
        >>. sepEndBy configEdit ws1
        .>> eof
        |>> fun edits -> edits |> Seq.fold (|>) Config.defaultJob

let parseJob (str : string) (name : string) =
    let result = runParserOnString Guts.jobParameters () name str
    match result with
    | Success (statements, _, _) -> statements
    | Failure (msg, _, _) -> failwith msg