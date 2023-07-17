module QueueTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, listOfLength, pair, string)
import Queue exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Queue module"
        [ test "list conversion is working collectly."
            (\_ ->
                let
                    inputList =
                        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ]
                in
                inputList
                    |> Queue.fromListLIFO
                    |> Queue.toListFIFO
                    |> Expect.equal inputList
            )
        , Test.fuzz (Fuzz.listOfLength 10 (pair bool int))
            "enqueue gives the same result as List."
            (\l ->
                let
                    queue =
                        List.foldl
                            (\( b, i ) q ->
                                if b then
                                    enqueue i q

                                else
                                    q
                            )
                            empty
                            l

                    lis =
                        List.foldl
                            (\( b, i ) li ->
                                if b then
                                    i :: li

                                else
                                    li
                            )
                            []
                            l
                in
                queue
                    --|> Debug.log "queue"
                    |> toListLIFO
                    |> Expect.equal lis
            )
        , Test.fuzz (Fuzz.listOfLength 1000 (pair bool int))
            "Random read and write gives the same result as List."
            (\l ->
                let
                    start =
                        List.range 1 10

                    queueHelp li q outL =
                        case li of
                            [] ->
                                ( q, outL )

                            ( b, i ) :: rest ->
                                if b then
                                    queueHelp rest (enqueue i q) (head q :: outL)

                                else
                                    queueHelp rest (dequeue q) (head q :: outL)

                    queue =
                        queueHelp l (fromListFIFO start) []

                    listHelp rst soFar out =
                        case rst of
                            [] ->
                                ( soFar, out )

                            ( b, i ) :: rest ->
                                if b then
                                    listHelp rest (i :: soFar) (List.head (List.drop (List.length soFar - 1) soFar) :: out)

                                else
                                    listHelp rest (List.take (List.length soFar - 1) soFar) (List.head (List.drop (List.length soFar - 1) soFar) :: out)

                    lis =
                        listHelp l start []
                in
                queue
                    |> Tuple.mapFirst toListLIFO
                    |> Expect.equal lis
            )
        ]
