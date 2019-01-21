module EasyBox.BoxCode
open FSharp.Data.UnitSystems.SI.UnitSymbols
open CNCFingers
open System

type BoxGenerator(box : BoxConfig) =
    let safeZ = 0.01<m>
    let tool = box.Tool
    let di = tool.Diameter
    let rad = di * 0.5
    let feed = tool.FeedRate
    let plunge = tool.PlungeRate
    let doc = tool.DepthOfCut
    let fingerParameters =
        let (_, _, z) = box.ExteriorDimensions
        // Need to make we have pockets of at least the lid thickness, so the front clearance works out.
        // We also need an even # of fingers, so round down to even by clearing the low bit.
        let fingerCount = 6
        if fingerCount < 2 then
            failwith "Box dimensions don't allow for reasonable fingers"
        {   Count = fingerCount
            Multipass = false
            SideAllowance = 0.004 * 0.0254<m>
            EndAllowance = 0.006 * 0.0254<m>
            ShortcutThickness = 0.0<m>
            SpoilDepth = 0.0<m>
            FuzzCut = 0.0<m>
        }

    let bottomSlotWidth =
        box.BottomThickness / 3.0 + box.SlotClearance * 2.0

    let bottomSlotDepth =
        box.SideThickness / 2.0

    let scaleFeed (forDoc : float<m>) =
        if forDoc <= 0.0<m> then feed else
        let scale = doc / forDoc
        if scale < 1.0 then feed
        else feed * (min tool.MaxScaleFeedRate scale)

    // All side boards, we are looking down at what will be INSIDE face of the board.
    // Arrows in diagrams point in the direction of the TOP (lid) of the box.

    // All boards will be zeroed centered on the front left corner and will be cut all the way around to ensure accurate dimensions.

    //     +------+
    //     |      |
    //    0,0-----+

    let zPasses start finish =
        if finish > start then failwith "Nonsense, shouldn't make z-passes bottom to top"
        seq {
            let mutable previous = start
            for z in start .. -doc .. finish do
                yield z, feed
                previous <- z
            if previous <> finish then
                yield finish, scaleFeed (abs (finish - previous))
        }

    let rangeWithLast start step finish =
        seq {
            let mutable even = true
            let mutable previous = start
            for x in start .. step .. finish do
                yield even, x
                previous <- x
                even <- not even
            if previous <> finish then
                yield even, finish
        }

    /// Cuts a rectangle whose bottom left corner is at x,y
    /// and with width and height as given.
    let cutRectangularProfile
        (bottomZ : float<m>)
        (x : float<m>, y : float<m>)
        (w : float<m>, h : float<m>) =
        seq {
            yield RapidMove [ Z, safeZ ]
            yield RapidMove [ X, x - rad; Y, y - rad ]
            for z, feed in zPasses -doc bottomZ do
                yield Move(plunge, [ Z, z ])
                yield Move(feed, [ X, x + w + rad ])
                yield Move(feed, [ Y, y + h + rad ])
                yield Move(feed, [ X, x - rad ])
                yield Move(feed, [ Y, y - rad ])
        }
    
    let cutPocket
        (bottomZ : float<m>)
        (x : float<m>, y : float<m>)
        (w : float<m>, h : float<m>) =
        seq {
            let step = di * 0.4
            let xLeft = x + rad
            let xRight = x + w - rad
            let yBot = y + rad
            let yTop = y + h - rad
            yield RapidMove [ Z, safeZ ]
            yield RapidMove [ X, xLeft; Y, yBot ]
            for z, feed in zPasses -doc bottomZ do
                yield Move(plunge, [ Z, z ])
                if w > h then
                    for even, yPass in rangeWithLast yBot step yTop do
                        yield Move(feed, [ Y, yPass ])
                        yield Move(feed, [ X, if even then xRight else xLeft ])
                else
                    for even, xPass in rangeWithLast xLeft step xRight do
                        yield Move(feed, [ X, xPass])
                        yield Move(feed, [ Y, if even then yTop else yBot ])
                yield RapidMove [ X, xLeft; Y, yBot ]
            yield RapidMove [ Z, safeZ ]
        }

    member this.FrontSide() =
        // Outputs:

        // _-_-_-
        // |    |
        // |    |
        // | <- |
        // |    |
        // |    |
        // -_-_-_

        let (boxX, _, boxZ) = box.ExteriorDimensions
        let xFormFarFingers =
            // box will start at rad, rad, so move an extra rad inwards to match
            // finger generation zero expectations (bit tangent to edges)
            GCodeTransform.translate (di, di, 0.0<m>)
        let xFormNearFingers =
            GCodeTransform.mirrorY
            // x will be same, Y needs to be the end of the board minus the tool radius
            >> GCodeTransform.translate (di, boxX, 0.0<m>)
        let fingerInstructions =
            {   Board = { Width = boxZ; Thickness = box.SideThickness }
                Tool = box.Tool
                Finger = fingerParameters
                Start = PocketThenFinger
                Copies = [ id ]
                SingleFile = true
                Machine = box.Machine
            } |> FingerCode.instructions |> Seq.toArray
        let cutForLid = box.LidThickness + box.SlotClearance
        seq {
            yield! cutRectangularProfile -box.SideThickness (rad + cutForLid, rad) (boxZ - cutForLid, boxX)
            yield! fingerInstructions |> Seq.map xFormNearFingers
            yield! fingerInstructions |> Seq.map xFormFarFingers

            let slotPosition =
                ( rad + boxZ - box.BottomThickness - box.SlotClearance
                , rad + box.SideThickness / 2.0
                )

            let slotDimensions =
                ( bottomSlotWidth
                , boxX - box.SideThickness
                )

            yield! cutPocket -bottomSlotDepth slotPosition slotDimensions
        }

