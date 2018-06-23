module Fingerator.Config
open FSharp.Data.UnitSystems.SI.UnitSymbols
open CNCFingers

let defaultJob =
    {   Tool =
            {   Diameter = 0.0<m>
                StepOver = 0.2
                DepthOfCut = 0.0<m>
                FeedRate = 0.0<m/s>
                PlungeRate = 0.0<m/s>
                RampFactor = 2.0
            }
        Board =
            {   Width = 0.0<m>
                Thickness = 0.0<m>
            }
        Finger =
            {   Count = 0
                SideAllowance = 0.004 * 0.0254<m>
                EndAllowance = 0.004 * 0.0254<m>
                SpoilDepth = 0.0<m>
                KickoutThreshold = 0.0<m>
                FuzzCut = 0.00025<m> // 1/4 mm fast cut to trim fuzz
            }
        Machine =
            {   Unit = Millimeters
            }
        Start = FingerThenPocket
        Transform = id
    }

let validateJob (job : JobParameters) =
    let tool = job.Tool
    let board = job.Board
    let finger = job.Finger
    if tool.Diameter <= 0.0<m> || tool.Diameter >= 0.25<m> then
        Some "Tool diameter unspecified or out of range"
    elif tool.StepOver <= 0.0 || tool.StepOver > 1.0 then
        Some "Tool step-over is out of range (should be between 0 and 1)"
    elif tool.RampFactor <= 0.0 || tool.RampFactor > 10.0 then
        Some "Tool ramp factor is out of range (should be between 0 and 10)"
    elif tool.DepthOfCut <= 0.0<m> || tool.DepthOfCut >= tool.Diameter * 2.0 then
        Some "Depth of cut unspecified or out of range"
    elif tool.FeedRate <= 0.0<m/s> then
        Some "Feed rate unspecified or out of range"
    elif tool.PlungeRate <= 0.0<m/s> || tool.PlungeRate > tool.FeedRate then
        Some "Plunge rate unspecified or exceeds feed rate"
    elif board.Width <= 0.0<m> then
        Some "Board width unspecified or out of range"
    elif board.Thickness <= 0.0<m> then
        Some "Board thickness unspecified or out of range"
    elif finger.Count <= 0 || float finger.Count > round (board.Width / tool.Diameter) then
        Some "Finger count unspecified or out of range"
    elif finger.SpoilDepth <= 0.0<m> || finger.SpoilDepth >= 0.002<m> then
        Some "Finger spoil depth out of range"
    elif finger.FuzzCut < 0.0<m> || finger.FuzzCut > 0.001<m> then
        Some "Fuzz cut out of range"
    else
        None