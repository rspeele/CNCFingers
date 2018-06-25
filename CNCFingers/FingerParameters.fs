namespace CNCFingers
open FSharp.Data.UnitSystems.SI.UnitSymbols

type BoardParameters =
    {   Width : float<m>
        Thickness : float<m>
    }

/// Tool is assumed to be an end mill.
type ToolParameters =
    {   Diameter : float<m>
        /// As a fraction of tool diameter. Affects the smoothness of the top curve cuts.
        StepOver : float
        /// How deep to cut with each pass. Half the tool diameter is a good choice.
        DepthOfCut : float<m>
        /// Horizontal feed rate.
        FeedRate : float<m/s>
        /// Vertical feed rate.
        PlungeRate : float<m/s>
        /// As a multiple of tool diameter. 2.0 a good choice.
        RampFactor : float
        /// Feed rate will scale with depth of cut up to this maximum multiplier.
        /// e.g. suppose this is 3.0. A cut half as deep as the normal DOC will move 2x as fast,
        /// but a cut 1/10 as deep as the normal DOC will only move 3x as fast not 10x.
        MaxScaleFeedRate : float
    }

type FingerParameters =
    {   /// The sum of fingers and pockets that the board width will be divided into.
        Count : int
        /// If true, don't assume the starting/ending pocket is on the edge of the board. Disables
        /// knocking off the rounded corner.
        Multipass: bool
        /// Theoretical X-axis gap between the side of a finger and the wall of the cutout it fits into.
        SideAllowance : float<m>
        /// Theoretical Y-axis gap between the curved top of a finger and the curved end of the cutout it fits into.
        /// This is taken off the top of the finger, not out of the cutout.
        /// Generally it's a good idea to go at least 2-3x the side allowance, especially since
        /// your Z-axis zero is probably a hair above the work.
        EndAllowance : float<m>
        /// How far below the work to go when cutting out the fingers.
        /// 0 is a "safe" default in case you are mounting your board directly on the
        /// machine bed (not a good idea in any case, but I could see somebody attempting it).
        /// Best is to mount your board on to of a "spoil board", set this to 4 thou or so, and expect to get a few
        /// shallow cuts in the spoil board.
        SpoilDepth : float<m>
        /// How thin an onion skin remaining at which we should try to kick out the "cut out" section of a big finger.
        /// Usually the cut out section just goes flying at some point and doesn't bother anyone, but it can sometimes
        /// lean over and interfere with cutting the curves. So it's good to try to get rid of it explicitly.
        /// 0 or less disables the kickout step.
        KickoutThreshold : float<m>
        /// There tends to be fuzz after cutting the dado. Clear it with a very thin cut -- this depth.
        FuzzCut : float<m>
    }

/// Run two jobs with the same other parameters but this flipped, in order to make two boards that mate together.
type JobStart =
    | FingerThenPocket
    | PocketThenFinger
    override this.ToString() =
        match this with
        | FingerThenPocket -> "FingerThenPocket"
        | PocketThenFinger -> "PocketThenFinger"

[<NoEquality>]
[<NoComparison>]
type JobParameters =
    {   Board : BoardParameters
        Tool : ToolParameters
        Finger : FingerParameters
        Machine : Machine
        Start : JobStart
        /// Multiple instances, each with their own transform to position them somewhere on the machine bed.
        Copies : (Instruction -> Instruction) list
    }