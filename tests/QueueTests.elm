module QueueTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List.Extra
import Queue exposing (Queue)
import Test exposing (..)


suite : Test
suite =
    describe "The Queue module"
        [ Test.fuzz operationsFuzzer
            "Checking random read-and-write."
            (\( initialList, operations ) ->
                let
                    queueOutput =
                        queueHelp operations
                            { queue = Queue.fromListFIFO initialList
                            , youngestSnapShots = []
                            , oldestSnapShots = []
                            }

                    listOutput =
                        listHelp operations
                            { queue = initialList
                            , youngestSnapShots = []
                            , oldestSnapShots = []
                            }

                    revQueueOutput =
                        queueHelp operations
                            { queue = Queue.fromListLIFO initialList
                            , youngestSnapShots = []
                            , oldestSnapShots = []
                            }

                    revListOutput =
                        listHelp operations
                            { queue = List.reverse initialList
                            , youngestSnapShots = []
                            , oldestSnapShots = []
                            }
                in
                ( queueOutput, revQueueOutput )
                    |> Expect.equal ( listOutput, revListOutput )
            )
        ]


type alias Output =
    { finalContents : List Int
    , finalContentsReversed : List Int
    , youngestSnapShots : List (Maybe Int)
    , oldestSnapShots : List (Maybe Int)
    , finalLength : Int
    }


type Operation
    = Enqueue Int
    | Dequeue


operationsFuzzer : Fuzzer ( List Int, List Operation )
operationsFuzzer =
    Fuzz.oneOfValues [ Enqueue 0, Dequeue ]
        |> Fuzz.andThen
            (\o ->
                case o of
                    Dequeue ->
                        Fuzz.constant Dequeue

                    Enqueue _ ->
                        Fuzz.int
                            |> Fuzz.map Enqueue
            )
        |> Fuzz.listOfLength 400
        |> Fuzz.pair (Fuzz.list Fuzz.int)


type alias IntermediateQueue =
    { queue : Queue Int
    , youngestSnapShots : List (Maybe Int)
    , oldestSnapShots : List (Maybe Int)
    }


queueHelp : List Operation -> IntermediateQueue -> Output
queueHelp operationsToBeDone soFar =
    case operationsToBeDone of
        [] ->
            { finalContents = Queue.toListLIFO soFar.queue
            , finalContentsReversed = Queue.toListFIFO soFar.queue
            , youngestSnapShots = soFar.youngestSnapShots
            , oldestSnapShots = soFar.oldestSnapShots
            , finalLength = Queue.length soFar.queue
            }

        operation :: rest ->
            let
                newQueue =
                    case operation of
                        Dequeue ->
                            Queue.dequeue soFar.queue

                        Enqueue x ->
                            Queue.enqueue x soFar.queue
            in
            queueHelp rest
                { queue = newQueue
                , youngestSnapShots =
                    Queue.rear newQueue :: soFar.youngestSnapShots
                , oldestSnapShots =
                    Queue.head newQueue :: soFar.oldestSnapShots
                }


type alias IntermediateList =
    { queue : List Int
    , youngestSnapShots : List (Maybe Int)
    , oldestSnapShots : List (Maybe Int)
    }


listHelp : List Operation -> IntermediateList -> Output
listHelp operationsToBeDone soFar =
    case operationsToBeDone of
        [] ->
            { finalContents = soFar.queue
            , finalContentsReversed = List.reverse soFar.queue
            , youngestSnapShots = soFar.youngestSnapShots
            , oldestSnapShots = soFar.oldestSnapShots
            , finalLength = List.length soFar.queue
            }

        operation :: rest ->
            let
                newQueue =
                    case operation of
                        Dequeue ->
                            soFar.queue
                                |> List.Extra.init
                                |> Maybe.withDefault []

                        Enqueue x ->
                            x :: soFar.queue
            in
            listHelp rest
                { queue = newQueue
                , youngestSnapShots =
                    List.head newQueue :: soFar.youngestSnapShots
                , oldestSnapShots =
                    List.Extra.last newQueue :: soFar.oldestSnapShots
                }
