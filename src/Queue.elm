module Queue exposing
    ( Queue
    , empty, fromListLIFO, fromListFIFO
    , isEmpty, head, rear, length
    , enqueue, dequeue
    , toListLIFO, toListFIFO
    )

{-| One way queue with time-complexity of enqueueing and dequeueing O(1) even at worst case.


# Type

@docs Queue


# Creation

@docs empty, fromListLIFO, fromListFIFO


# Query

@docs isEmpty, head, rear, length


# Modification

@docs enqueue, dequeue


# Deconstruction

@docs toListLIFO, toListFIFO

-}

{- ABOUT IMPLEMENTATION
    * Read the paper before reading this code.
    * data structures are named as following diagram.

            entrance(`enqueue`)     |                 exit(`head`)
         -------↓ -------------------------------------↑-------
            `thrd`(Leaf x)          |                 `frst`
    `left`  `scnd`(Leaf x)       `Layer`              `scnd`  'right'
   `Column` `frst`(Leaf x)          |                 `thrd` `Column`
         -------↓ -------------------------------------↑-------
            `thrd`<- `Branch` with  |                 `frst`
            `scnd`       2 `Leaf`s  |                 `scnd`
            `frst`                  |                 `thrd`
         -------↓ -------------------------------------↑-------
            `thrd`<- `Branch` with  |                 `frst`
            `scnd`       4 `Leaf`s  |                 `scnd`
            `frst`                  |                 `thrd`
         -------↓ -------------------------------------↑-------
                …                   |                  …
                ↓                   |                  ↑
                `thrd`→`scnd`→`frst`→`thrd`→`scnd`→`frst`
                                 Bottom

    * Each layer is colored with element counts according to the table below.
        |left＼right|    0    |    1    |    2    |    3    |
        |   ---:    |  :---:  |  :---:  |  :---:  |  :---:  |
        |     0     |   Red   | Yellow  |  Green  |  Green  |
        |     1     |   Red   | Yellow  |  Green  |  Green  |
        |     2     |   Red   | Yellow  | Yellow  | Yellow  |
        |     3     |   Red   |   Red   |   Red   |   Red   |

    * Every red layer can be greenized in one operation
        with the color of neiboring under layers possibly getting bad.
        (See `fixOnlyLeft` function for example.)
    * The stacking layers are further organized as `Segment`s.
        Each `Segment` is composed of one heading red or green layer and
        following zero or more yellow layers,
        or single layer named Bottom.
-}
{- PERFORMANCE NOTES
   This module is intendet to be used as a low-level tool which is required to have stable and sufficiently good performance.
   So the author paid full attention on run-time performance,
   especially on following points.

   * Every inner function is designed to be unary in avoidance of
       compiled JS code having `A2(...)` function call.
   * All custom types has exactly one data to benefit from "hidden class" generation in some JS engine.
   * No `case` expression is used in `let` clause because that usage leads to generation of immediate-function like `var _v0 = (function(){...})()` in compiled JS code.
   * All `if` expression compares literal integer in order not to call high-cost comparison function for elmish data structures.
   * Some of the process is inlined to reduce JS function call.
-}


type Node a
    = None ()
    | Leaf a
    | Branch { junior : Node a, senior : Node a }


none : Node a
none =
    None ()


type alias Column a =
    { frst : Node a
    , scnd : Node a
    , thrd : Node a
    , cnt : Int
    }


emptyColumn : Column x
emptyColumn =
    { frst = none, scnd = none, thrd = none, cnt = 0 }


type alias Layer a =
    { left : Column a
    , right : Column a
    }


emptyLayer : Layer x
emptyLayer =
    { left = emptyColumn, right = emptyColumn }


type alias LayerPair x =
    { above : Layer x
    , below : Layer x
    }


emptyLayerPair : LayerPair x
emptyLayerPair =
    { above = emptyLayer, below = emptyLayer }


type YellowLayers a
    = YCons { yhead : Layer a, yrest : YellowLayers a }
    | YNil ()


ynil : YellowLayers a
ynil =
    YNil ()


type Segment a
    = Segment
        { head : Layer a
        , tail : YellowLayers a
        , nextSegment : Segment a
        }
    | Bottom (Layer a)


{-| Returns empty queue.

    head empty == Nothing

-}
empty : Queue x
empty =
    Queue (Bottom emptyLayer)


{-| Returns `True` if the argument has no element.

    empty
        |> enqueue "a"
        |> isEmpty
    --> False

    empty
        |> enqueue "a"
        |> dequeue
        |> isEmpty
    --> True

-}
isEmpty : Queue x -> Bool
isEmpty (Queue s) =
    case s of
        Segment _ ->
            False

        Bottom btm ->
            btm.left.cnt + btm.right.cnt == 0


{-| above.left.cnt == 3 && below.left.cnt <= 2.
-}
fixOnlyLeft : LayerPair x -> LayerPair x
fixOnlyLeft old =
    let
        newAbove =
            { left =
                { thrd = none
                , scnd = none
                , frst = old.above.left.thrd
                , cnt = 1
                }
            , right = old.above.right
            }

        passed =
            Branch
                { junior = old.above.left.scnd
                , senior = old.above.left.frst
                }

        newLeftBelow =
            if old.below.left.cnt == 0 then
                { thrd = none
                , scnd = none
                , frst = passed
                , cnt = 1
                }

            else if old.below.left.cnt == 1 then
                { thrd = none
                , scnd = passed
                , frst = old.below.left.frst
                , cnt = 2
                }

            else
                { thrd = passed
                , scnd = old.below.left.scnd
                , frst = old.below.left.frst
                , cnt = 3
                }

        newBelow =
            { left = newLeftBelow
            , right = old.below.right
            }
    in
    { above = newAbove, below = newBelow }


{-| above.right.cnt == 0 && below.right.cnt /= 0.
-}
fixOnlyRight : LayerPair x -> LayerPair x
fixOnlyRight old =
    case old.below.right.frst of
        Branch { junior, senior } ->
            let
                newRightBelow =
                    { thrd = none
                    , scnd = old.below.right.thrd
                    , frst = old.below.right.scnd
                    , cnt = old.below.right.cnt - 1
                    }

                newBelow =
                    { left = old.below.left
                    , right = newRightBelow
                    }
            in
            { below = newBelow
            , above =
                { left = old.above.left
                , right =
                    { thrd = none
                    , scnd = junior
                    , frst = senior
                    , cnt = 2
                    }
                }
            }

        _ ->
            emptyLayerPair


{-| above.left >= 2 && above.right.cnt <= 1 && below is yellow or green.
-}
fixLeftAndRight : LayerPair x -> LayerPair x
fixLeftAndRight old =
    case old.below.right.frst of
        Branch { junior, senior } ->
            let
                newLeftAbove =
                    { thrd = none
                    , scnd = none
                    , frst = old.above.left.thrd
                    , cnt = old.above.left.cnt - 2
                    }

                passed =
                    Branch
                        { junior = old.above.left.scnd
                        , senior = old.above.left.frst
                        }

                newLeftBelow =
                    if old.below.left.cnt == 0 then
                        { thrd = none
                        , scnd = none
                        , frst = passed
                        , cnt = 1
                        }

                    else if old.below.left.cnt == 1 then
                        { thrd = none
                        , scnd = passed
                        , frst = old.below.left.frst
                        , cnt = 2
                        }

                    else
                        { thrd = passed
                        , scnd = old.below.left.scnd
                        , frst = old.below.left.frst
                        , cnt = 3
                        }

                newRightBelow =
                    { thrd = none
                    , scnd = old.below.right.thrd
                    , frst = old.below.right.scnd
                    , cnt = old.below.right.cnt - 1
                    }

                newBelow =
                    { left = newLeftBelow
                    , right = newRightBelow
                    }
            in
            case old.above.right.cnt of
                0 ->
                    { below = newBelow
                    , above =
                        { left = newLeftAbove
                        , right =
                            { thrd = none
                            , scnd = junior
                            , frst = senior
                            , cnt = 2
                            }
                        }
                    }

                _ ->
                    { below = newBelow
                    , above =
                        { left = newLeftAbove
                        , right =
                            { thrd = junior
                            , scnd = senior
                            , frst = old.above.right.frst
                            , cnt = 3
                            }
                        }
                    }

        _ ->
            emptyLayerPair


fixBottom : LayerPair x -> Segment x
fixBottom lowestPair =
    let
        { above, below } =
            if lowestPair.above.left.cnt <= 1 then
                fixOnlyRight lowestPair

            else if lowestPair.above.right.cnt >= 2 then
                fixOnlyLeft lowestPair

            else
                fixLeftAndRight lowestPair

        newBottomSegment =
            if below.left.cnt == 3 then
                if below.right.cnt == 0 then
                    Bottom
                        { left = emptyColumn
                        , right = below.left
                        }

                else if below.right.cnt == 1 then
                    Bottom
                        { left =
                            { thrd = none
                            , scnd = none
                            , frst = below.left.thrd
                            , cnt = 1
                            }
                        , right =
                            { thrd = below.left.scnd
                            , scnd = below.left.frst
                            , frst = below.right.frst
                            , cnt = 3
                            }
                        }

                else if below.right.cnt == 2 then
                    Bottom
                        { left =
                            { thrd = none
                            , scnd = below.left.thrd
                            , frst = below.left.scnd
                            , cnt = 2
                            }
                        , right =
                            { thrd = below.left.frst
                            , scnd = below.right.scnd
                            , frst = below.right.frst
                            , cnt = 3
                            }
                        }

                else
                    Segment
                        { head =
                            { left = emptyColumn
                            , right =
                                { thrd = none
                                , scnd = below.right.scnd
                                , frst = below.right.frst
                                , cnt = 2
                                }
                            }
                        , tail = ynil
                        , nextSegment =
                            Bottom
                                { left = emptyColumn
                                , right =
                                    { thrd = none
                                    , scnd =
                                        Branch
                                            { junior =
                                                below.left.thrd
                                            , senior =
                                                below.left.scnd
                                            }
                                    , frst =
                                        Branch
                                            { junior =
                                                below.left.frst
                                            , senior =
                                                below.right.thrd
                                            }
                                    , cnt = 2
                                    }
                                }
                        }

            else if below.right.cnt == 0 then
                Bottom
                    { left = emptyColumn
                    , right = below.left
                    }

            else
                Bottom below
    in
    case newBottomSegment of
        Bottom e ->
            if e.left.cnt + e.right.cnt == 0 then
                Bottom above

            else
                Segment
                    { head = above
                    , tail = ynil
                    , nextSegment = newBottomSegment
                    }

        _ ->
            Segment
                { head = above
                , tail = ynil
                , nextSegment = newBottomSegment
                }


fixRedTail : { head : Layer x, tail : YellowLayers x, nextSegment : Segment x } -> Segment x
fixRedTail r =
    let
        fixFunc =
            if r.head.left.cnt <= 1 then
                fixOnlyRight

            else if r.head.right.cnt >= 2 then
                fixOnlyLeft

            else
                fixLeftAndRight
    in
    case r.tail of
        YCons { yhead, yrest } ->
            let
                { above, below } =
                    fixFunc { above = r.head, below = yhead }
            in
            if below.left.cnt == 3 || below.right.cnt == 0 then
                Segment
                    { head = above
                    , tail = ynil
                    , nextSegment =
                        Segment
                            { head = below
                            , tail = yrest
                            , nextSegment = r.nextSegment
                            }
                    }

            else
                Segment
                    { head = above
                    , tail =
                        YCons { yhead = below, yrest = yrest }
                    , nextSegment = r.nextSegment
                    }

        YNil () ->
            case r.nextSegment of
                Segment next ->
                    let
                        { above, below } =
                            fixFunc { above = r.head, below = next.head }
                    in
                    if below.left.cnt <= 1 && below.right.cnt >= 2 then
                        Segment
                            { head = above
                            , tail = ynil
                            , nextSegment =
                                Segment
                                    { head = below
                                    , tail = next.tail
                                    , nextSegment =
                                        next.nextSegment
                                    }
                            }

                    else
                        Segment
                            { head = above
                            , tail =
                                YCons
                                    { yhead = below
                                    , yrest = next.tail
                                    }
                            , nextSegment =
                                next.nextSegment
                            }

                Bottom bottom ->
                    fixBottom { above = r.head, below = bottom }


{-| Representation of First-In, First-Out(FIFO) data structure.
-}
type Queue a
    = Queue (Segment a)


{-| Returns the oldest element injected in the queue wrapped in `Maybe`.

    empty
        |> head
    --> Nothing

    empty
        |> enqueue "e"
        |> enqueue "l"
        |> enqueue "m"
        |> head
    --> Just "e"

-}
head : Queue a -> Maybe a
head (Queue segment) =
    case segment of
        Segment s ->
            case s.head.right.frst of
                Leaf a ->
                    Just a

                _ ->
                    Nothing

        Bottom b ->
            case b.right.frst of
                Leaf a ->
                    Just a

                _ ->
                    Nothing


{-| Returns queue without the oldest element of the given queue.

    empty
        |> enqueue "e"
        |> enqueue "l"
        |> enqueue "m"
        |> enqueue "i"
        |> enqueue "s"
        |> enqueue "h"
        |> dequeue
        |> head
    --> Just "l"

-}
dequeue : Queue a -> Queue a
dequeue (Queue s) =
    case s of
        Segment oldSegment ->
            let
                newHeadRight =
                    { thrd = none
                    , scnd = oldSegment.head.right.thrd
                    , frst = oldSegment.head.right.scnd
                    , cnt = oldSegment.head.right.cnt - 1
                    }

                toBeFixed =
                    { head =
                        { left = oldSegment.head.left
                        , right = newHeadRight
                        }
                    , tail = oldSegment.tail
                    , nextSegment = oldSegment.nextSegment
                    }
            in
            if newHeadRight.cnt == 0 then
                Queue (fixRedTail toBeFixed)

            else if toBeFixed.head.left.cnt <= 1 && newHeadRight.cnt >= 2 then
                Queue (Segment toBeFixed)

            else
                case toBeFixed.nextSegment of
                    Segment next ->
                        if next.head.left.cnt == 3 || next.head.right.cnt == 0 then
                            Queue
                                (Segment
                                    { head = toBeFixed.head
                                    , tail = toBeFixed.tail
                                    , nextSegment =
                                        fixRedTail next
                                    }
                                )

                        else
                            Queue (Segment toBeFixed)

                    Bottom _ ->
                        Queue (Segment toBeFixed)

        Bottom oldBottom ->
            let
                newRight =
                    { thrd = none
                    , scnd = oldBottom.right.thrd
                    , frst = oldBottom.right.scnd
                    , cnt = oldBottom.right.cnt - 1
                    }

                newBottom =
                    if newRight.cnt <= 0 then
                        { left = emptyColumn
                        , right = oldBottom.left
                        }

                    else
                        { left = oldBottom.left
                        , right = newRight
                        }
            in
            Queue (Bottom newBottom)


{-| Returns the queue as second argument with the element as the first argument injected into the rear side.

    empty
        |> enqueue 100
        |> enqueue 200
        |> head
    --> Just 100

-}
enqueue : x -> Queue x -> Queue x
enqueue x (Queue s) =
    case s of
        Segment oldSegment ->
            let
                newHeadLeft =
                    if oldSegment.head.left.cnt == 0 then
                        { thrd = none
                        , scnd = none
                        , frst = Leaf x
                        , cnt = 1
                        }

                    else if oldSegment.head.left.cnt == 1 then
                        { thrd = none
                        , scnd = Leaf x
                        , frst = oldSegment.head.left.frst
                        , cnt = 2
                        }

                    else
                        { thrd = Leaf x
                        , scnd = oldSegment.head.left.scnd
                        , frst = oldSegment.head.left.frst
                        , cnt = 3
                        }

                toBeFixed =
                    { head =
                        { left = newHeadLeft
                        , right = oldSegment.head.right
                        }
                    , tail = oldSegment.tail
                    , nextSegment = oldSegment.nextSegment
                    }
            in
            if newHeadLeft.cnt == 3 then
                Queue (fixRedTail toBeFixed)

            else if newHeadLeft.cnt <= 1 && toBeFixed.head.right.cnt >= 2 then
                Queue (Segment toBeFixed)

            else
                case toBeFixed.nextSegment of
                    Segment next ->
                        if next.head.left.cnt == 3 || next.head.right.cnt == 0 then
                            Queue
                                (Segment
                                    { head = toBeFixed.head
                                    , tail = toBeFixed.tail
                                    , nextSegment =
                                        fixRedTail next
                                    }
                                )

                        else
                            Queue (Segment toBeFixed)

                    Bottom _ ->
                        Queue (Segment toBeFixed)

        Bottom oldBottom ->
            let
                newLayer =
                    if oldBottom.right.cnt == 0 then
                        { left = emptyColumn
                        , right =
                            if oldBottom.left.cnt == 0 then
                                { thrd = none
                                , scnd = none
                                , frst = Leaf x
                                , cnt = 1
                                }

                            else if oldBottom.left.cnt == 1 then
                                { thrd = none
                                , scnd = Leaf x
                                , frst = oldBottom.left.frst
                                , cnt = 2
                                }

                            else
                                { thrd = Leaf x
                                , scnd = oldBottom.left.scnd
                                , frst = oldBottom.left.frst
                                , cnt = 3
                                }
                        }

                    else if oldBottom.right.cnt == 1 then
                        if oldBottom.left.cnt == 0 then
                            { left = emptyColumn
                            , right =
                                { thrd = none
                                , scnd = Leaf x
                                , frst = oldBottom.right.frst
                                , cnt = 2
                                }
                            }

                        else if oldBottom.left.cnt == 1 then
                            { left = emptyColumn
                            , right =
                                { thrd = Leaf x
                                , scnd = oldBottom.left.frst
                                , frst = oldBottom.right.frst
                                , cnt = 3
                                }
                            }

                        else
                            { left =
                                { thrd = none
                                , scnd = none
                                , frst = Leaf x
                                , cnt = 1
                                }
                            , right =
                                { thrd = oldBottom.left.scnd
                                , scnd = oldBottom.left.frst
                                , frst = oldBottom.right.frst
                                , cnt = 3
                                }
                            }

                    else if oldBottom.right.cnt == 2 then
                        if oldBottom.left.cnt == 0 then
                            { left = emptyColumn
                            , right =
                                { thrd = Leaf x
                                , scnd = oldBottom.right.scnd
                                , frst = oldBottom.right.frst
                                , cnt = 3
                                }
                            }

                        else if oldBottom.left.cnt == 1 then
                            { left =
                                { thrd = none
                                , scnd = none
                                , frst = Leaf x
                                , cnt = 1
                                }
                            , right =
                                { thrd = oldBottom.left.frst
                                , scnd = oldBottom.right.scnd
                                , frst = oldBottom.right.frst
                                , cnt = 3
                                }
                            }

                        else
                            { left =
                                { thrd = none
                                , scnd = Leaf x
                                , frst = oldBottom.left.scnd
                                , cnt = 2
                                }
                            , right =
                                { thrd = oldBottom.left.frst
                                , scnd = oldBottom.right.scnd
                                , frst = oldBottom.right.frst
                                , cnt = 3
                                }
                            }

                    else
                        { left =
                            if oldBottom.left.cnt == 0 then
                                { thrd = none
                                , scnd = none
                                , frst = Leaf x
                                , cnt = 1
                                }

                            else if oldBottom.left.cnt == 1 then
                                { thrd = none
                                , scnd = Leaf x
                                , frst = oldBottom.left.frst
                                , cnt = 2
                                }

                            else
                                { thrd = Leaf x
                                , scnd = oldBottom.left.scnd
                                , frst = oldBottom.left.frst
                                , cnt = 3
                                }
                        , right = oldBottom.right
                        }

                newBottom =
                    if newLayer.left.cnt == 3 && newLayer.right.cnt == 3 then
                        Segment
                            { head =
                                { left = emptyColumn
                                , right =
                                    { thrd = none
                                    , scnd = oldBottom.right.scnd
                                    , frst = oldBottom.right.frst
                                    , cnt = 2
                                    }
                                }
                            , tail = ynil
                            , nextSegment =
                                Bottom
                                    { left = emptyColumn
                                    , right =
                                        { thrd = none
                                        , scnd =
                                            Branch
                                                { junior =
                                                    newLayer.left.thrd
                                                , senior =
                                                    newLayer.left.scnd
                                                }
                                        , frst =
                                            Branch
                                                { junior =
                                                    newLayer.left.frst
                                                , senior =
                                                    oldBottom.right.thrd
                                                }
                                        , cnt = 2
                                        }
                                    }
                            }

                    else
                        Bottom newLayer
            in
            Queue newBottom


{-| Create queue from list.
The newest element comes head of the queue.

    ["H","e","l","l","o"]
        |> fromListLIFO
        |> head
    --> Just "H"

-}
fromListLIFO : List x -> Queue x
fromListLIFO l =
    fromListHelp l empty


{-| Create queue from list.
The oldest element of the list comes head of the queue.

    ["H"]
        |> (::) "e"
        |> (::) "l"
        |> (::) "l"
        |> (::) "o"
        |> fromListFIFO
        |> head
    --> Just "H"

-}
fromListFIFO : List x -> Queue x
fromListFIFO l =
    fromListHelp (List.reverse l) empty


fromListHelp : List x -> Queue x -> Queue x
fromListHelp l q =
    case l of
        h :: t ->
            fromListHelp t (enqueue h q)

        [] ->
            q


{-| Convert a queue to List. The last element in the queue will become the head of the returned list.

    empty
        |> enqueue 100
        |> enqueue 200
        |> enqueue 300
        |> toListLIFO
    --> [300,200,100]

-}
toListLIFO : Queue x -> List x
toListLIFO q =
    toListHelp q []


{-| Convert a queue to List. The first element in the queue will become the head of the returned list.

    empty
        |> enqueue 100
        |> enqueue 200
        |> enqueue 300
        |> toListFIFO
    --> [100,200,300]

-}
toListFIFO : Queue x -> List x
toListFIFO q =
    toListHelp q []
        |> List.reverse


toListHelp : Queue x -> List x -> List x
toListHelp q l =
    case head q of
        Nothing ->
            l

        Just h ->
            toListHelp (dequeue q) (h :: l)


{-| Returns how many elements is contained in the queue. The time complexity is _O(log N)_.

    empty
        |> length
    --> 0

    empty
        |> enqueue 1
        |> enqueue 2
        |> enqueue 3
        |> dequeue
        |> length
    --> 2

-}
length : Queue x -> Int
length (Queue segments) =
    lengthHelp segments { currentWeight = 1, countedSoFar = 0 }


lengthHelp : Segment x -> { currentWeight : Int, countedSoFar : Int } -> Int
lengthHelp segments { currentWeight, countedSoFar } =
    case segments of
        Segment s ->
            { countedSoFar =
                countedSoFar
                    + currentWeight
                    * (s.head.left.cnt + s.head.right.cnt)
            , currentWeight = currentWeight * 2
            }
                |> lengthHelpHelp s.tail
                |> lengthHelp s.nextSegment

        Bottom btm ->
            countedSoFar
                + currentWeight
                * (btm.left.cnt + btm.right.cnt)


lengthHelpHelp : YellowLayers x -> { currentWeight : Int, countedSoFar : Int } -> { currentWeight : Int, countedSoFar : Int }
lengthHelpHelp yellowLayers ({ currentWeight, countedSoFar } as intermediate) =
    case yellowLayers of
        YCons { yhead, yrest } ->
            lengthHelpHelp yrest
                { countedSoFar =
                    countedSoFar
                        + currentWeight
                        * (yhead.left.cnt + yhead.right.cnt)
                , currentWeight = currentWeight * 2
                }

        _ ->
            intermediate


{-| Returns the newest element if exists, wrapped in `Maybe`.
Worst-case time complexity of this operation is _O(log N)_.

    empty
        |> enqueue 100
        |> enqueue 200
        |> rear
    --> Just 200

    empty
        |> enqueue 100
        |> dequeue
        |> rear
    --> Nothing

-}
rear : Queue x -> Maybe x
rear (Queue segment) =
    rearHelp segment


rearHelp : Segment x -> Maybe x
rearHelp segment =
    case segment of
        Segment s ->
            if s.head.left.cnt == 0 then
                case s.tail of
                    YNil () ->
                        rearHelp s.nextSegment

                    YCons { yhead, yrest } ->
                        { head = yhead
                        , tail = yrest
                        , nextSegment = s.nextSegment
                        }
                            |> Segment
                            |> rearHelp

            else
                s.head.left
                    |> takeColumnRearNode
                    |> takeYoungestLeafInNode

        Bottom btm ->
            let
                targetColumn =
                    if btm.left.cnt == 0 then
                        btm.right

                    else
                        btm.left
            in
            targetColumn
                |> takeColumnRearNode
                |> takeYoungestLeafInNode


takeColumnRearNode : Column x -> Node x
takeColumnRearNode { thrd, scnd, frst, cnt } =
    if cnt == 3 then
        thrd

    else if cnt == 2 then
        scnd

    else
        frst


takeYoungestLeafInNode : Node x -> Maybe x
takeYoungestLeafInNode n =
    case n of
        Branch { junior } ->
            takeYoungestLeafInNode junior

        Leaf x ->
            Just x

        None () ->
            Nothing
