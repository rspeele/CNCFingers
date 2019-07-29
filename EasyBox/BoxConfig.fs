namespace EasyBox
open FSharp.Data.UnitSystems.SI.UnitSymbols
open CNCFingers

// Specifies a box with a sliding lid.
// Will output separate g-code files for:
//
//     Back (w/ slot for bottom)
//     Left side (w/ slot for bottom and slot all the way to front for lid)
//     Right side (w/ slot for bottom and slot all the way to front for lid)
//     Front (slot for bottom, shorter than others for lid clearance)


// The bottom can be really thick, thick enough to machine pockets out of (generated in a separate program).

type SlotLidConfig =
    {   LidThickness : float<m> // Should be close to side thickness but boards may vary.
        SlotClearance : float<m>
    }

type LidType =
    | SlidingLid of SlotLidConfig
    | CaptiveLid of SlotLidConfig
    

type BoxType =
    {   BottomCaptive : SlotLidConfig option
        Top : LidType option
    }

type BoxConfig =
    {   Tool : ToolParameters
        Machine : Machine
        SideThickness : float<m>
        BoxType : BoxType

        /// board width * expansion factor is max width we expect the board to grow to (for bottom, front <-> back)
        WoodExpansionFactor : float

        //   +--------+
        //  /        /|
        // /        / |
        //+--------+  |
        //|        |  |
        //|      Z |  +
        //|        | /  Y
        //|        |/
        //+--------+
        //    X
        ExteriorDimensions : float<m> * float<m> * float<m>
        ForceFingerCount : int option
    }

