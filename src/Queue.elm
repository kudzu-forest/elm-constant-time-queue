module Queue exposing
    ( Queue
    , empty, fromListLIFO, fromListFIFO
    , isEmpty, isEqualTo, head, rear, length
    , enqueue, dequeue, map
    , toListLIFO, toListFIFO, fold
    )

{-| This module provides functionalities as one-way queue. The worst-case time complexity of `enqueue` and `dequeue` is _O(1)_.


# Types

@docs Queue


# Creation

@docs empty, fromListLIFO, fromListFIFO


# Query

@docs isEmpty, isEqualTo, head, rear, length


# Update

@docs enqueue, dequeue, map


# Deconstruction

@docs toListLIFO, toListFIFO, fold

-}

import Bitwise


type Element a
    = Nil
    | Leaf a
    | Branch { young : Element a, old : Element a }


type alias GreenOrYellowContent a =
    { p1 : Element a -- prefix
    , p2 : Element a -- prefix
    , pCnt : Int
    , tail : YellowTail a
    , next : Substack a
    , sNew : Element a -- suffix
    , sMid : Element a
    , sOld : Element a
    , sCnt : Int
    }


type alias RedContent a =
    { p1 : Element a
    , p2 : Element a
    , pCnt : Int
    , child : GreenOrYellowContent a
    , tail : YellowTail a
    , next : Substack a
    , sNew : Element a -- suffix
    , sMid : Element a
    , sOld : Element a
    , sCnt : Int
    }


type Substack a
    = Green (GreenOrYellowContent a)
    | Red (RedContent a)
    | End


type YellowTail a
    = YCons (GreenOrYellowContent a)
    | YNil


{-| Represents one-way queue for any contents.
-}
type Queue a
    = Queue (GreenOrYellowContent a)


emptyGreen : GreenOrYellowContent a
emptyGreen =
    { p1 = Nil
    , p2 = Nil
    , pCnt = 0
    , tail = YNil
    , next = End
    , sNew = Nil
    , sMid = Nil
    , sOld = Nil
    , sCnt = 0
    }


{-| Represents queue with no element.

    empty |> isEmpty --> True

    empty |> head --> Nothing

    empty |> rear --> Nothing

-}
empty : Queue a
empty =
    Queue emptyGreen


fix : RedContent a -> GreenOrYellowContent a
fix r =
    let
        p1 =
            r.p1

        p2 =
            Nil

        pCnt =
            Bitwise.and 1 r.pCnt

        cp1 =
            if r.pCnt <= 1 then
                r.child.p1

            else if r.child.pCnt == 1 then
                Nil

            else
                r.p2

        cp2 =
            if r.pCnt >= 2 && r.child.pCnt == 1 then
                Branch { young = r.p2, old = r.child.p1 }

            else
                r.child.p2

        cpCnt =
            Bitwise.shiftRightBy 1 r.pCnt + r.child.pCnt

        sCnt =
            Bitwise.or 2 r.sCnt

        csCnt =
            Bitwise.shiftRightBy 1 r.sCnt + r.child.sCnt - 1

        csNew =
            if r.sCnt <= 1 then
                Nil

            else
                r.child.sNew

        csMid =
            if r.sCnt <= 1 then
                r.child.sNew

            else
                r.child.sMid

        csOld =
            if r.sCnt <= 1 then
                r.child.sMid

            else
                r.child.sOld
    in
    case r.child.sOld of
        Branch b ->
            let
                sNew =
                    if r.sCnt == 1 then
                        b.young

                    else
                        r.sNew

                sMid =
                    if r.sCnt >= 2 then
                        r.sMid

                    else if r.sCnt == 1 then
                        b.old

                    else
                        b.young

                sOld =
                    if r.sCnt >= 1 then
                        r.sOld

                    else
                        b.old
            in
            if cpCnt /= 3 && csCnt /= 0 then
                let
                    newTail =
                        if cpCnt <= 1 && csCnt >= 2 then
                            YNil

                        else
                            YCons
                                { p1 = cp1
                                , p2 = cp2
                                , pCnt = cpCnt
                                , tail = r.tail
                                , next = End
                                , sNew = csNew
                                , sMid = csMid
                                , sOld = csOld
                                , sCnt = csCnt
                                }

                    newNext =
                        if cpCnt <= 1 && csCnt >= 2 then
                            Green
                                { p1 = cp1
                                , p2 = Nil
                                , pCnt = cpCnt
                                , tail = r.tail
                                , next = r.next
                                , sNew = csNew
                                , sMid = csMid
                                , sOld = csOld
                                , sCnt = csCnt
                                }

                        else
                            r.next
                in
                { p1 = p1
                , p2 = p2
                , pCnt = pCnt
                , tail = newTail
                , next = newNext
                , sNew = sNew
                , sMid = sMid
                , sOld = sOld
                , sCnt = sCnt
                }

            else
                case r.tail of
                    YCons y ->
                        { p1 = p1
                        , p2 = p2
                        , pCnt = pCnt
                        , tail = YNil
                        , next =
                            Red
                                { p1 = cp1
                                , p2 = cp2
                                , pCnt = cpCnt
                                , child = y
                                , tail = y.tail
                                , next = r.next
                                , sNew = csNew
                                , sMid = csMid
                                , sOld = csOld
                                , sCnt = csCnt
                                }
                        , sNew = sNew
                        , sMid = sMid
                        , sOld = sOld
                        , sCnt = sCnt
                        }

                    YNil ->
                        case r.next of
                            Green g ->
                                { p1 = p1
                                , p2 = p2
                                , pCnt = pCnt
                                , tail = YNil
                                , next =
                                    Red
                                        { p1 = cp1
                                        , p2 = cp2
                                        , pCnt = cpCnt
                                        , child = g
                                        , tail = g.tail
                                        , next = g.next
                                        , sNew = csNew
                                        , sMid = csMid
                                        , sOld = csOld
                                        , sCnt = csCnt
                                        }
                                , sNew = sNew
                                , sMid = sMid
                                , sOld = sOld
                                , sCnt = sCnt
                                }

                            _ ->
                                -- r.next must be End.
                                let
                                    newTail =
                                        if cpCnt == 1 then
                                            YCons
                                                { p1 = Nil
                                                , p2 = Nil
                                                , pCnt = 0
                                                , tail = YNil
                                                , next = End
                                                , sNew = Nil
                                                , sMid = Nil
                                                , sOld = cp1
                                                , sCnt = 1
                                                }

                                        else
                                            YNil
                                in
                                if cpCnt <= 1 || csCnt >= 2 then
                                    let
                                        newNext =
                                            if cpCnt <= 1 then
                                                End

                                            else
                                                -- if csCnt >= 2 then
                                                Green
                                                    { p1 = cp1
                                                    , p2 = Nil
                                                    , pCnt = 1
                                                    , tail =
                                                        YCons
                                                            { p1 = Nil
                                                            , p2 = Nil
                                                            , pCnt = 0
                                                            , tail = YNil
                                                            , next = End
                                                            , sNew = Nil
                                                            , sMid = Nil
                                                            , sOld = cp2
                                                            , sCnt = 1
                                                            }
                                                    , next = End
                                                    , sNew = csNew
                                                    , sMid = csMid
                                                    , sOld = csOld
                                                    , sCnt = csCnt
                                                    }
                                    in
                                    { p1 = p1
                                    , p2 = p2
                                    , pCnt = pCnt
                                    , tail = newTail
                                    , next = newNext
                                    , sNew = sNew
                                    , sMid = sMid
                                    , sOld = sOld
                                    , sCnt = sCnt
                                    }

                                else
                                    case cp2 of
                                        Branch br ->
                                            let
                                                newNext =
                                                    Green
                                                        { p1 = cp1
                                                        , p2 = Nil
                                                        , pCnt = cpCnt - 2
                                                        , tail = YNil
                                                        , next = End
                                                        , sNew =
                                                            if csCnt == 1 then
                                                                br.young

                                                            else
                                                                Nil
                                                        , sMid =
                                                            if csCnt == 1 then
                                                                br.old

                                                            else
                                                                br.young
                                                        , sOld =
                                                            if csCnt == 1 then
                                                                csOld

                                                            else
                                                                br.old
                                                        , sCnt = csCnt + 2
                                                        }
                                            in
                                            { p1 = p1
                                            , p2 = p2
                                            , pCnt = pCnt
                                            , tail = newTail
                                            , next = newNext
                                            , sNew = sNew
                                            , sMid = sMid
                                            , sOld = sOld
                                            , sCnt = sCnt
                                            }

                                        _ ->
                                            --impossible
                                            emptyGreen

        _ ->
            emptyGreen


{-| Returns a queue with the given element attached on the rear side.

    empty
        |> enqueue "e"
        -- first in
        |> enqueue "l"
        |> enqueue "m"
        |> toListFIFO
            -->
            [ "e", "l", "m" ]

-}
enqueue : a -> Queue a -> Queue a
enqueue a (Queue q) =
    let
        l =
            Leaf a
    in
    if q.pCnt == 0 then
        { p1 = l
        , p2 = Nil
        , pCnt = 1
        , tail = q.tail
        , next = q.next
        , sNew = q.sNew
        , sMid = q.sMid
        , sOld = q.sOld
        , sCnt = q.sCnt
        }
            |> Queue

    else if q.pCnt == 1 then
        let
            p2 =
                Branch { young = l, old = q.p1 }
        in
        case q.next of
            Red r ->
                { p1 = Nil
                , p2 = p2
                , pCnt = 2
                , tail = q.tail
                , next = Green (fix r)
                , sNew = q.sNew
                , sMid = q.sMid
                , sOld = q.sOld
                , sCnt = q.sCnt
                }
                    |> Queue

            _ ->
                { p1 = Nil
                , p2 = p2
                , pCnt = 2
                , tail = q.tail
                , next = q.next
                , sNew = q.sNew
                , sMid = q.sMid
                , sOld = q.sOld
                , sCnt = q.sCnt
                }
                    |> Queue

    else
        case q.tail of
            YCons y ->
                fix
                    { p1 = Leaf a
                    , p2 = q.p2
                    , pCnt = 3
                    , child = y
                    , tail = y.tail
                    , next = q.next
                    , sNew = q.sNew
                    , sMid = q.sMid
                    , sOld = q.sOld
                    , sCnt = q.sCnt
                    }
                    |> Queue

            YNil ->
                case q.next of
                    Green g ->
                        fix
                            { p1 = l
                            , p2 = q.p2
                            , pCnt = 3
                            , child = g
                            , tail = g.tail
                            , next = g.next
                            , sNew = q.sNew
                            , sMid = q.sMid
                            , sOld = q.sOld
                            , sCnt = q.sCnt
                            }
                            |> Queue

                    _ ->
                        -- q.next must be End
                        if q.sCnt <= 1 then
                            case q.p2 of
                                Branch br ->
                                    (if q.sCnt == 1 then
                                        { p1 = l
                                        , p2 = Nil
                                        , pCnt = 1
                                        , tail = YNil
                                        , next = End
                                        , sNew = br.young
                                        , sMid = br.old
                                        , sOld = q.sOld
                                        , sCnt = 3
                                        }

                                     else
                                        { p1 = l
                                        , p2 = Nil
                                        , pCnt = 1
                                        , tail = YNil
                                        , next = End
                                        , sNew = Nil
                                        , sMid = br.young
                                        , sOld = br.old
                                        , sCnt = 2
                                        }
                                    )
                                        |> Queue

                                _ ->
                                    --impossible
                                    empty

                        else
                            { p1 = l
                            , p2 = Nil
                            , pCnt = 1
                            , tail =
                                YCons
                                    { p1 = Nil
                                    , p2 = Nil
                                    , pCnt = 0
                                    , tail = YNil
                                    , next = End
                                    , sNew = Nil
                                    , sMid = Nil
                                    , sOld = q.p2
                                    , sCnt = 1
                                    }
                            , next = End
                            , sNew = q.sNew
                            , sMid = q.sMid
                            , sOld = q.sOld
                            , sCnt = q.sCnt
                            }
                                |> Queue


{-| Returns a queue with the oldest element removed.

    intermediateQueue =
        empty
            |> enqueue "e" -- this will be removed
            |> enqueue "l" -- first-in element in the final queue.
            |> enqueue "m"

    finalQueue =
        dequeue intermediateQueue

    finalQueue
        |> toListFIFO -->
            [ "l"
            , "m"
            ]

-}
dequeue : Queue a -> Queue a
dequeue (Queue q) =
    if q.sCnt == 3 then
        Queue
            { p1 = q.p1
            , p2 = q.p2
            , pCnt = q.pCnt
            , tail = q.tail
            , next = q.next
            , sNew = Nil
            , sMid = q.sNew
            , sOld = q.sMid
            , sCnt = 2
            }

    else if q.sCnt == 2 then
        case q.next of
            Red r ->
                Queue
                    { p1 = q.p1
                    , p2 = q.p2
                    , pCnt = q.pCnt
                    , tail = q.tail
                    , next = Green (fix r)
                    , sNew = Nil
                    , sMid = Nil
                    , sOld = q.sMid
                    , sCnt = 1
                    }

            _ ->
                Queue
                    { p1 = q.p1
                    , p2 = q.p2
                    , pCnt = q.pCnt
                    , tail = q.tail
                    , next = q.next
                    , sNew = Nil
                    , sMid = Nil
                    , sOld = q.sMid
                    , sCnt = 1
                    }

    else if q.sCnt == 1 then
        case q.tail of
            YCons y ->
                fix
                    { p1 = q.p1
                    , p2 = q.p2
                    , pCnt = q.pCnt
                    , child = y
                    , tail = y.tail
                    , next = q.next
                    , sNew = Nil
                    , sMid = Nil
                    , sOld = Nil
                    , sCnt = 0
                    }
                    |> Queue

            YNil ->
                case q.next of
                    Green g ->
                        fix
                            { p1 = q.p1
                            , p2 = q.p2
                            , pCnt = q.pCnt
                            , child = g
                            , tail = g.tail
                            , next = g.next
                            , sNew = Nil
                            , sMid = Nil
                            , sOld = Nil
                            , sCnt = 0
                            }
                            |> Queue

                    _ ->
                        { p1 = q.p1
                        , p2 = q.p2
                        , pCnt = q.pCnt
                        , tail = YNil
                        , next = End
                        , sNew = Nil
                        , sMid = Nil
                        , sOld = Nil
                        , sCnt = 0
                        }
                            |> Queue

    else
        case q.p2 of
            Branch br ->
                { p1 = Nil
                , p2 = Nil
                , pCnt = 0
                , tail = YNil
                , next = End
                , sNew = Nil
                , sMid = Nil
                , sOld = br.young
                , sCnt = 1
                }
                    |> Queue

            _ ->
                empty


{-| Returns the oldest element in the queue wrapped in `Maybe`. Takes _O(1)_ time.

    queue : Queue String
    queue =
        empty
            |> enqueue "e"
            |> enqueue "l"
            |> enqueue "m"

    head queue --> Just "e"

    head empty --> Nothing

-}
head : Queue a -> Maybe a
head (Queue q) =
    case q.sOld of
        Leaf a ->
            Just a

        _ ->
            case q.p2 of
                Branch b ->
                    case b.old of
                        Leaf a ->
                            Just a

                        _ ->
                            Nothing

                _ ->
                    case q.p1 of
                        Leaf a ->
                            Just a

                        _ ->
                            Nothing


{-| Returns the newest element in the queue wrapped in `Maybe`. Takes _O(log_n)\_ time at worst case.

    queue : Queue String
    queue =
        empty
            |> enqueue "e"
            |> enqueue "l"
            |> enqueue "m"

    rear queue --> Just "m"

    rear empty --> Nothing

-}
rear : Queue a -> Maybe a
rear (Queue q) =
    rearHelp
        { p1 = q.p1
        , p2 = q.p2
        , p3 = Nil
        , p4 = Nil
        , tail = q.tail
        , next = q.next
        , sNew = q.sNew
        , sMid = q.sMid
        , sOld = q.sOld
        }


rearHelp :
    { p1 : Element a
    , p2 : Element a
    , p3 : Element a
    , p4 : Element a
    , tail : YellowTail a
    , next : Substack a
    , sNew : Element a
    , sMid : Element a
    , sOld : Element a
    }
    -> Maybe a
rearHelp o =
    case o.p1 of
        Nil ->
            case o.p2 of
                Nil ->
                    case o.p3 of
                        Nil ->
                            case o.p4 of
                                Nil ->
                                    case o.tail of
                                        YNil ->
                                            case o.next of
                                                End ->
                                                    case o.sNew of
                                                        Nil ->
                                                            case o.sMid of
                                                                Nil ->
                                                                    getYoungestInElement o.sOld

                                                                _ ->
                                                                    getYoungestInElement o.sMid

                                                        _ ->
                                                            getYoungestInElement o.sNew

                                                Green g ->
                                                    rearHelp
                                                        { p1 = g.p1
                                                        , p2 = g.p2
                                                        , p3 = Nil
                                                        , p4 = Nil
                                                        , tail = g.tail
                                                        , next = g.next
                                                        , sNew = g.sNew
                                                        , sMid = g.sMid
                                                        , sOld = g.sOld
                                                        }

                                                Red r ->
                                                    rearHelp
                                                        { p1 = r.p1
                                                        , p2 = r.p2
                                                        , p3 = r.child.p1
                                                        , p4 = r.child.p2
                                                        , tail = r.tail
                                                        , next = r.next
                                                        , sNew = r.child.sNew
                                                        , sMid = r.child.sMid
                                                        , sOld = r.child.sOld
                                                        }

                                        YCons y ->
                                            rearHelp
                                                { p1 = y.p1
                                                , p2 = y.p2
                                                , p3 = Nil
                                                , p4 = Nil
                                                , tail = y.tail
                                                , next = o.next
                                                , sNew = y.sNew
                                                , sMid = y.sMid
                                                , sOld = y.sOld
                                                }

                                _ ->
                                    getYoungestInElement o.p4

                        _ ->
                            getYoungestInElement o.p3

                _ ->
                    getYoungestInElement o.p2

        _ ->
            getYoungestInElement o.p1


getYoungestInElement : Element a -> Maybe a
getYoungestInElement e =
    case e of
        Branch b ->
            getYoungestInElement b.young

        Leaf a ->
            Just a

        Nil ->
            Nothing


{-| Returns `True` if and only if the queue has no element.

    empty
        |> isEmpty
            -->
            True

    empty
        |> enqueue "test"
        |> isEmpty
            -->
            False

-}
isEmpty : Queue a -> Bool
isEmpty q =
    case head q of
        Nothing ->
            True

        _ ->
            False


{-| Converts a `List` to a `Queue` in a way that last-in element in the list becomes first-out element in the queue.

    list : List String
    list =
        []
            |> (::) "m"
            |> (::) "l"
            |> (::) "e" -- Here is last-in element.

    queue : Queue String
    queue =
        fromListLIFO list

    head queue --> Just "e"

    rear queue --> Just "m"

-}
fromListLIFO : List a -> Queue a
fromListLIFO l =
    fromListLIFOHelp l empty


fromListLIFOHelp : List a -> Queue a -> Queue a
fromListLIFOHelp l sofar =
    case l of
        [] ->
            sofar

        h :: t ->
            fromListLIFOHelp t (enqueue h sofar)


{-| Converts a `List` to a `Queue` in a way that first-in element in the list becomes first-out element in the queue.

    list : List String
    list =
        []
            |> (::) "m" -- Here is first-in element.
            |> (::) "l"
            |> (::) "e"

    queue : Queue String
    queue =
        fromListFIFO list

    head queue --> Just "m"

    rear queue --> Just "e"

-}
fromListFIFO : List a -> Queue a
fromListFIFO l =
    fromListLIFOHelp (List.reverse l) empty


{-| Converts a `Queue` to a `List` in a way that last-in element in the queue becomes first-out element in the list.

    queue : Queue String
    queue =
        empty
            |> enqueue "e"
            |> enqueue "l"
            |> enqueue "m" --Here is last-in element.

    toListLIFO queue --> ["m","l","e"]

-}
toListLIFO : Queue a -> List a
toListLIFO q =
    toListLIFOHelp q []


toListLIFOHelp : Queue a -> List a -> List a
toListLIFOHelp q sofar =
    case head q of
        Nothing ->
            sofar

        Just a ->
            toListLIFOHelp (dequeue q) (a :: sofar)


{-| Converts a `Queue` to a `List` in a way that first-in element in the queue becomes first-out element in the list.

    queue : Queue String
    queue =
        empty
            |> enqueue "e" --Here is last-in element.
            |> enqueue "l"
            |> enqueue "m"

    toListFIFO queue --> ["e","l","m"]

-}
toListFIFO : Queue a -> List a
toListFIFO q =
    List.reverse (toListLIFOHelp q [])


{-| Compare the contents and the order in two queues. Returns `True` if and only if all the elements contained and the orders are the same.
Note that comparison in `==` may return false even if the contents are the same, because there is some redunduncy in the inner structure of the queue. Takes _O(n)_ time.

    q1 : Queue String
    q1 =
        empty
            |> enqueue "e"
            |> enqueue "l"
            |> enqueue "m"
            |> dequeue

    q2 : Queue String
    q2 =
        empty
            |> enqueue "e"
            |> dequeue
            |> enqueue "l"
            |> enqueue "m"

    q3 : Queue String
    q3 =
        empty
            |> enqueue "m"
            |> enqueue "l"
            |> enqueue "e"
            |> dequeue

    q1 |> isEqualTo q2 --> True

    q1 |> isEqualTo q3 --> False

-}
isEqualTo : Queue a -> Queue a -> Bool
isEqualTo q1 q2 =
    case head q1 of
        Just h1 ->
            case head q2 of
                Just h2 ->
                    if h1 == h2 then
                        isEqualTo
                            (dequeue q1)
                            (dequeue q2)

                    else
                        False

                Nothing ->
                    False

        Nothing ->
            case head q2 of
                Just _ ->
                    False

                Nothing ->
                    True


{-| Returns how many elements the Queue has. Takes _O(log_n)\_ time.
-}
length : Queue a -> Int
length (Queue q) =
    lengthHelp 2 (q.pCnt + q.sCnt) q.tail q.next


lengthHelp : Int -> Int -> YellowTail a -> Substack a -> Int
lengthHelp weight sofar tail next =
    case tail of
        YCons y ->
            lengthHelp (Bitwise.shiftLeftBy 1 weight)
                ((y.pCnt + y.sCnt) * weight + sofar)
                y.tail
                next

        YNil ->
            case next of
                Green g ->
                    lengthHelp (Bitwise.shiftLeftBy 1 weight)
                        ((g.pCnt + g.sCnt) * weight + sofar)
                        g.tail
                        g.next

                Red r ->
                    lengthHelp (Bitwise.shiftLeftBy 2 weight)
                        (((r.child.pCnt + r.child.sCnt) * 2 + r.pCnt + r.sCnt) * weight + sofar)
                        r.tail
                        r.next

                End ->
                    sofar


{-| produces one value that sums up the all elements in the queue.
The fold function is applied in the order of insertion.

     ["e", "l", "m"]
        |> fromListLIFO
        |> fold (++) "" --> "mle"

-}
fold : (c -> a -> a) -> a -> Queue c -> a
fold f acc q =
    case head q of
        Just crr ->
            fold f (f crr acc) (dequeue q)

        Nothing ->
            acc


{-| Updates all elements in the `Queue` with given function.

     [1, 2, 3]
        |> fromListLIFO
        |> map (\x -> x * x)
        |> map (String.fromInt)
        |> toListLIFO --> ["9","4","1"]

-}
map : (a -> b) -> Queue a -> Queue b
map f q =
    mapHelp f q empty


mapHelp : (a -> b) -> Queue a -> Queue b -> Queue b
mapHelp f qa qb =
    case head qa of
        Just a ->
            mapHelp f (dequeue qa) (enqueue (f a) qb)

        Nothing ->
            qb
