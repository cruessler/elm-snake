module Tests exposing (indexedMap, legalMovements)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Snake as Snake exposing (Direction(..), Snake)
import Test exposing (Test, describe, test)


initialSnake : Snake
initialSnake =
    Snake.initialize ( 5, 1 ) 5


legalMovements : Test
legalMovements =
    describe "legal movements"
        [ test "moving back has no effect" <|
            \_ ->
                let
                    movedSnake =
                        Snake.move Left initialSnake
                in
                Expect.equal movedSnake (Ok initialSnake)
        , test "moving in any other direction moves the snake’s head" <|
            \_ ->
                let
                    otherDirections =
                        [ ( Up, ( 5, 0 ) ), ( Right, ( 6, 1 ) ), ( Down, ( 5, 2 ) ) ]

                    expectations =
                        otherDirections
                            |> List.map
                                (\( direction, expectedHead ) ->
                                    Snake.move direction
                                        >> Result.map Snake.head
                                        >> Expect.equal (Ok expectedHead)
                                )
                in
                Expect.all expectations initialSnake
        ]


indexedMap : Test
indexedMap =
    describe "indexedMap"
        [ test "for initial snake" <|
            \_ ->
                Expect.equal
                    (Snake.mapSegments identity initialSnake)
                    [ ( 5, 1 ), ( 4, 1 ), ( 3, 1 ), ( 2, 1 ), ( 1, 1 ), ( 0, 1 ) ]
        , test "for snake that has moved" <|
            \_ ->
                let
                    movedSnake =
                        initialSnake
                            |> Snake.move Down
                            |> Result.andThen (Snake.move Down)

                    segments =
                        movedSnake
                            |> Result.map (Snake.mapSegments identity)
                in
                Expect.equal
                    segments
                    (Ok
                        [ ( 5, 3 ), ( 5, 2 ), ( 5, 1 ), ( 4, 1 ), ( 3, 1 ), ( 2, 1 ) ]
                    )
        ]
