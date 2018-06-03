namespace CNCFingers
open FSharp.Data.UnitSystems.SI.UnitSymbols

type BoardParameters =
    {   Width : float<m>
        Thickness : float<m>
    }

/// Tool is assumed to be an end mill.
type ToolParameters =
    {   Diameter : float<m>
        /// As a percentage of tool diameter.
        StepOver : float
        /// How deep to cut with each pass. Half the tool diameter is a good choice.
        DepthOfCut : float<m>
        /// Horizontal feed rate.
        FeedRate : float<m/s>
        /// Vertical feed rate.
        PlungeRate : float<m/s>
    }

type FingerParameters =
    {   /// Finger width BEFORE trimming for allowances. Board width should be FingerWidth * N where N is an integer.
        FingerWidth : float<m>
        /// Theoretical X-axis gap between the side of a finger and the wall of the cutout it fits into.
        SideAllowance : float<m>
        /// Theoretical Y-axis gap between the curved top of a finger and the curved end of the cutout it fits into.
        /// This is taken off the top of the finger, not out of the cutout.
        EndAllowance : float<m>
    }

/// Run two jobs with the same other parameters but this flipped, in order to make two boards that mate together.
type JobStart =
    | FingerThenPocket
    | PocketThenFinger

type JobParameters =
    {   Board : BoardParameters
        Tool : ToolParameters
        Finger : FingerParameters
        Start : JobStart
    }