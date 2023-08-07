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

   * All custom types has exactly one data to benefit from "hidden class" generation in some JS engine.
   * No `case` expression is used in `let` clause because that usage leads to generation of immediate-function like `var _v0 = (function(){...})()` in compiled JS code.
   * All `if` expression compares literal integer in order not to call high-cost comparison function for elmish data structures.
   * Some of the process is inlined to reduce JS function call.
-}


type Node x
    = None ()
    | Leaf x
    | Branch { left : Node x, right : Node x }


type alias Column x =
    { thrd : Node x
    , scnd : Node x
    , frst : Node x
    , cnt : Int
    }


type alias Layer x =
    { left : Column x
    , right : Column x
    }


type alias YellowContent x =
    { head : Layer x, tail : YellowLayers x }


type YellowLayers x
    = YNil ()
    | YCons (YellowContent x)


type alias SegmentContent x =
    { head : Layer x
    , tail : YellowLayers x
    , next : Segment x
    }


type Segment x
    = Segment (SegmentContent x)
    | Bottom ()


{-| Representation of First-In, First-Out(FIFO) data structure.
-}
type Queue x
    = Queue (SegmentContent x)


emptyNode : Node x
emptyNode =
    None ()


emptyColumn : Column x
emptyColumn =
    Column emptyNode emptyNode emptyNode 0


emptyLayer : Layer x
emptyLayer =
    Layer emptyColumn emptyColumn


emptyYellowContent : YellowContent x
emptyYellowContent =
    { head = emptyLayer, tail = emptyYellowLayers }


emptyYellowLayers : YellowLayers x
emptyYellowLayers =
    YNil ()


bottom : Segment x
bottom =
    Bottom ()


emptySegmentContent : SegmentContent x
emptySegmentContent =
    { head = emptyLayer
    , tail = emptyYellowLayers
    , next = bottom
    }


{-| Returns empty queue.

    head empty == Nothing

-}
empty : Queue x
empty =
    Queue emptySegmentContent


fix : Layer x -> Layer x -> YellowLayers x -> Segment x -> SegmentContent x
fix above below tail next =
    case below.right.frst of
        Branch b ->
            let
                newAbove =
                    { left =
                        if above.left.cnt >= 2 then
                            { thrd = emptyNode
                            , scnd = emptyNode
                            , frst = above.left.thrd
                            , cnt = above.left.cnt - 2
                            }

                        else
                            above.left
                    , right =
                        if above.right.cnt >= 2 then
                            above.right

                        else if above.right.cnt == 1 then
                            { thrd = b.left
                            , scnd = b.right
                            , frst = above.right.frst
                            , cnt = 3
                            }

                        else
                            { thrd = emptyNode
                            , scnd = b.left
                            , frst = b.right
                            , cnt = 2
                            }
                    }

                downNode =
                    Branch
                        { left = above.left.scnd
                        , right = above.left.frst
                        }

                newBelow =
                    { left =
                        if above.left.cnt >= 2 then
                            if below.left.cnt == 2 then
                                { thrd = downNode
                                , scnd = below.left.scnd
                                , frst = below.left.frst
                                , cnt = 3
                                }

                            else if below.left.cnt == 1 then
                                { thrd = emptyNode
                                , scnd = downNode
                                , frst = below.left.frst
                                , cnt = 2
                                }

                            else
                                { thrd = emptyNode
                                , scnd = emptyNode
                                , frst = downNode
                                , cnt = 1
                                }

                        else
                            below.left
                    , right =
                        if above.right.cnt >= 2 then
                            below.right

                        else
                            { thrd = emptyNode
                            , scnd = below.right.thrd
                            , frst = below.right.scnd
                            , cnt = below.right.cnt - 1
                            }
                    }
            in
            case next of
                Segment _ ->
                    if
                        (newBelow.left.cnt == 3)
                            || (newBelow.right.cnt == 0)
                            || ((newBelow.left.cnt <= 1)
                                    && (newBelow.right.cnt >= 2)
                               )
                    then
                        { head = newAbove
                        , tail = emptyYellowLayers
                        , next =
                            Segment
                                { head = newBelow
                                , tail = tail
                                , next = next
                                }
                        }

                    else
                        { head = newAbove
                        , tail =
                            YCons
                                { head = newBelow
                                , tail = tail
                                }
                        , next = next
                        }

                Bottom () ->
                    let
                        btm =
                            fixBottom newBelow
                    in
                    { head = newAbove
                    , tail = emptyYellowLayers
                    , next =
                        if btm.head.right.cnt == 0 then
                            bottom

                        else
                            Segment btm
                    }

        _ ->
            emptySegmentContent


fixBottom : Layer x -> SegmentContent x
fixBottom btm =
    case btm.right.cnt of
        3 ->
            if btm.left.cnt == 3 then
                { head =
                    { left = emptyColumn
                    , right =
                        { thrd = emptyNode
                        , scnd = btm.right.scnd
                        , frst = btm.right.frst
                        , cnt = 2
                        }
                    }
                , tail = emptyYellowLayers
                , next =
                    { head =
                        { left = emptyColumn
                        , right =
                            { thrd = emptyNode
                            , scnd =
                                Branch
                                    { left = btm.left.thrd
                                    , right = btm.left.scnd
                                    }
                            , frst =
                                Branch
                                    { left = btm.left.frst
                                    , right = btm.right.thrd
                                    }
                            , cnt = 2
                            }
                        }
                    , tail = emptyYellowLayers
                    , next = bottom
                    }
                        |> Segment
                }

            else
                { head = btm
                , tail = emptyYellowLayers
                , next = bottom
                }

        2 ->
            { head =
                { left =
                    if btm.left.cnt <= 1 then
                        emptyColumn

                    else
                        { thrd = emptyNode
                        , scnd = btm.left.thrd
                        , frst = btm.left.scnd
                        , cnt = btm.left.cnt - 1
                        }
                , right =
                    { thrd = btm.left.frst
                    , scnd = btm.right.scnd
                    , frst = btm.right.frst
                    , cnt =
                        if btm.left.cnt == 0 then
                            2

                        else
                            3
                    }
                }
            , tail = emptyYellowLayers
            , next = bottom
            }

        1 ->
            { head =
                { left =
                    if btm.left.cnt <= 2 then
                        emptyColumn

                    else
                        { thrd = emptyNode
                        , scnd = emptyNode
                        , frst = btm.left.scnd
                        , cnt = btm.left.cnt - 2
                        }
                , right =
                    { thrd = btm.left.scnd
                    , scnd = btm.left.frst
                    , frst = btm.right.frst
                    , cnt =
                        if btm.left.cnt >= 2 then
                            3

                        else
                            1 + btm.left.cnt
                    }
                }
            , tail = emptyYellowLayers
            , next = bottom
            }

        _ ->
            { head =
                { left =
                    emptyColumn
                , right =
                    btm.left
                }
            , tail = emptyYellowLayers
            , next = bottom
            }


{-| Inject a new element into the rear side of the queue.

    empty
        |> enqueue 100
        |> enqueue 200
        |> head
    --> Just 100

-}
enqueue : x -> Queue x -> Queue x
enqueue x (Queue old) =
    let
        l =
            old.head.left

        newLeftCnt =
            l.cnt + 1

        newLeft =
            if newLeftCnt == 3 then
                { thrd = Leaf x
                , scnd = l.scnd
                , frst = l.frst
                , cnt = 3
                }

            else if newLeftCnt == 2 then
                { thrd = emptyNode
                , scnd = Leaf x
                , frst = l.frst
                , cnt = 2
                }

            else
                { thrd = emptyNode
                , scnd = emptyNode
                , frst = Leaf x
                , cnt = 1
                }

        newHead =
            { left = newLeft
            , right = old.head.right
            }
    in
    if newLeftCnt == 1 then
        { head = newHead
        , tail = old.tail
        , next = old.next
        }
            |> Queue

    else if newLeftCnt == 3 then
        case old.next of
            Segment next ->
                case old.tail of
                    YCons y ->
                        fix
                            newHead
                            y.head
                            y.tail
                            old.next
                            |> Queue

                    YNil () ->
                        fix
                            newHead
                            next.head
                            next.tail
                            next.next
                            |> Queue

            Bottom () ->
                Queue (fixBottom newHead)

    else
        case old.next of
            Segment next ->
                if
                    (next.head.left.cnt == 3)
                        || (next.head.right.cnt == 0)
                then
                    case next.tail of
                        YCons y ->
                            let
                                fixed =
                                    fix
                                        next.head
                                        y.head
                                        y.tail
                                        next.next
                            in
                            Queue
                                { head = newHead
                                , tail = old.tail
                                , next = Segment fixed
                                }

                        YNil () ->
                            case next.next of
                                Segment nextnext ->
                                    let
                                        fixed =
                                            fix
                                                next.head
                                                nextnext.head
                                                nextnext.tail
                                                nextnext.next
                                    in
                                    Queue
                                        { head = newHead
                                        , tail = old.tail
                                        , next = Segment fixed
                                        }

                                Bottom () ->
                                    Queue
                                        { head = newHead
                                        , tail = old.tail
                                        , next = old.next
                                        }

                else
                    Queue
                        { head = newHead
                        , tail = old.tail
                        , next = old.next
                        }

            Bottom () ->
                Queue (fixBottom newHead)


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
dequeue : Queue x -> Queue x
dequeue (Queue old) =
    if old.head.right.cnt == 0 then
        if old.head.left.cnt == 0 then
            empty

        else
            Queue (fixBottom old.head)
                |> dequeue

    else
        let
            r =
                old.head.right

            newRightCnt =
                r.cnt - 1

            newRight =
                if newRightCnt == 0 then
                    emptyColumn

                else
                    { thrd = emptyNode
                    , scnd = r.thrd
                    , frst = r.scnd
                    , cnt = newRightCnt
                    }

            newHead =
                { left = old.head.left
                , right = newRight
                }
        in
        if newRightCnt == 2 then
            Queue
                { head = newHead
                , tail = old.tail
                , next = old.next
                }

        else if newRightCnt == 0 then
            case old.next of
                Segment next ->
                    case old.tail of
                        YCons y ->
                            fix
                                newHead
                                y.head
                                y.tail
                                old.next
                                |> Queue

                        YNil () ->
                            fix
                                newHead
                                next.head
                                next.tail
                                next.next
                                |> Queue

                Bottom () ->
                    Queue (fixBottom newHead)

        else
            case old.next of
                Segment next ->
                    if
                        (next.head.left.cnt == 3)
                            || (next.head.right.cnt == 0)
                    then
                        case next.tail of
                            YCons y ->
                                let
                                    fixed =
                                        fix
                                            next.head
                                            y.head
                                            y.tail
                                            next.next
                                in
                                Queue
                                    { head = newHead
                                    , tail = old.tail
                                    , next = Segment fixed
                                    }

                            YNil () ->
                                case next.next of
                                    Segment nextnext ->
                                        let
                                            fixed =
                                                fix
                                                    next.head
                                                    nextnext.head
                                                    nextnext.tail
                                                    nextnext.next
                                        in
                                        Queue
                                            { head = newHead
                                            , tail = old.tail
                                            , next = Segment fixed
                                            }

                                    Bottom () ->
                                        Queue
                                            { head = newHead
                                            , tail = old.tail
                                            , next = old.next
                                            }

                    else
                        Queue
                            { head = newHead
                            , tail = old.tail
                            , next = old.next
                            }

                Bottom () ->
                    Queue (fixBottom newHead)


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
isEmpty (Queue sgm) =
    (sgm.head.right.cnt == 0)
        && (sgm.head.left.cnt == 0)


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
head : Queue x -> Maybe x
head (Queue sgm) =
    case sgm.head.right.frst of
        Leaf x ->
            Just x

        _ ->
            case sgm.head.left.frst of
                Leaf x ->
                    Just x

                _ ->
                    Nothing


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
rear (Queue sgm) =
    rearHelp sgm


rearHelp : SegmentContent x -> Maybe x
rearHelp sgm =
    case sgm.next of
        Segment next ->
            if sgm.head.left.cnt == 0 then
                case sgm.tail of
                    YCons y ->
                        let
                            maybeRear =
                                yellowRear y
                        in
                        case maybeRear of
                            Nothing ->
                                rearHelp next

                            _ ->
                                maybeRear

                    YNil () ->
                        rearHelp next

            else
                let
                    targetNode =
                        if sgm.head.left.cnt == 1 then
                            sgm.head.left.frst

                        else if sgm.head.left.cnt == 2 then
                            sgm.head.left.scnd

                        else
                            sgm.head.left.thrd
                in
                leftMostInNode targetNode

        Bottom () ->
            let
                targetNode =
                    if sgm.head.left.cnt == 3 then
                        sgm.head.left.thrd

                    else if sgm.head.left.cnt == 2 then
                        sgm.head.left.scnd

                    else if sgm.head.left.cnt == 1 then
                        sgm.head.left.frst

                    else if sgm.head.right.cnt == 3 then
                        sgm.head.right.thrd

                    else if sgm.head.right.cnt == 2 then
                        sgm.head.right.scnd

                    else
                        sgm.head.right.frst
            in
            leftMostInNode targetNode


yellowRear : YellowContent x -> Maybe x
yellowRear y =
    if y.head.left.cnt == 0 then
        case y.tail of
            YCons nextY ->
                yellowRear nextY

            YNil () ->
                Nothing

    else
        let
            rearNode =
                if y.head.left.cnt == 2 then
                    y.head.left.scnd

                else
                    y.head.left.frst
        in
        leftMostInNode rearNode


leftMostInNode : Node x -> Maybe x
leftMostInNode n =
    case n of
        Branch b ->
            leftMostInNode b.left

        Leaf x ->
            Just x

        None () ->
            Nothing


type LengthProcess
    = HeadLength
    | YellowTailLength


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
length (Queue sgm) =
    lengthHelp HeadLength 1 0 emptyYellowContent sgm


lengthHelp : LengthProcess -> Int -> Int -> YellowContent x -> SegmentContent x -> Int
lengthHelp process base accm y sgm =
    case process of
        HeadLength ->
            let
                updatedAccm =
                    (sgm.head.left.cnt + sgm.head.right.cnt)
                        * base
                        + accm
            in
            case sgm.tail of
                YCons yy ->
                    lengthHelp YellowTailLength
                        (base * 2)
                        updatedAccm
                        yy
                        sgm

                YNil () ->
                    case sgm.next of
                        Segment next ->
                            lengthHelp HeadLength
                                (base * 2)
                                updatedAccm
                                y
                                next

                        Bottom () ->
                            updatedAccm

        YellowTailLength ->
            let
                updatedAccm =
                    (y.head.left.cnt + y.head.right.cnt)
                        * base
                        + accm
            in
            case y.tail of
                YCons yy ->
                    lengthHelp YellowTailLength
                        (base * 2)
                        updatedAccm
                        yy
                        sgm

                YNil () ->
                    case sgm.next of
                        Segment next ->
                            lengthHelp HeadLength
                                (base * 2)
                                updatedAccm
                                y
                                next

                        Bottom () ->
                            -- impossible
                            -1


{-| Create queue from list.
The newest element comes head of the queue.

    [ "H" -- The Last-In element is "H"
    , "e"
    , "l"
    , "l"
    , "o"
    ]
        |> fromListLIFO
        |> head
    --> Just "H"

-}
fromListLIFO : List x -> Queue x
fromListLIFO l =
    List.foldl enqueue empty l


{-| Create queue from list.
The oldest element of the list comes head of the queue.

    []
        |> (::) "H" -- The First-In element is "H"
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
    List.foldr enqueue empty l


{-| Convert a queue to List. The Last-In element in the queue will become the head of the returned list.

    empty
        |> enqueue 100
        |> enqueue 200
        |> enqueue 300 -- The Last-In element is 300
        |> toListLIFO
    --> [300,200,100]

-}
toListLIFO : Queue x -> List x
toListLIFO q =
    toListHelp q []


{-| Convert a queue to List. The First-In element in the queue will become the head of the returned list.

    empty
        |> enqueue 100 -- The First-In element is 100
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
toListHelp q soFar =
    case head q of
        Just x ->
            toListHelp (dequeue q) (x :: soFar)

        Nothing ->
            soFar
