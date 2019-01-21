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

type BoxConfig =
    {   Tool : ToolParameters
        Machine : Machine
        LidThickness : float<m> // Should be close to side thickness but boards may vary.
        SideThickness : float<m>
        BottomThickness : float<m>

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
    }

