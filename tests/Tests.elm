module Tests exposing (illegalMovements, legalMovements, toList)

import Expect
import Snake as Snake exposing (Direction(..), IllegalMove(..), Snake)
import Test exposing (Test, describe, test)


initialSnake : Snake
initialSnake =
    Snake.initialize ( 5, 1 ) Nothing


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


illegalMovements : Test
illegalMovements =
    describe "illegal movements"
        [ test "running into itself results in an error" <|
            \_ ->
                let
                    okSnake =
                        initialSnake
                            |> Snake.move Down
                            |> Result.andThen (Snake.move Left)

                    errSnake =
                        okSnake
                            |> Result.andThen (Snake.move Up)
                in
                Expect.all
                    [ \_ ->
                        Expect.equal
                            (okSnake |> Result.map Snake.toList)
                            (Ok [ ( 4, 2 ), ( 5, 2 ), ( 5, 1 ), ( 4, 1 ), ( 3, 1 ), ( 2, 1 ) ])
                    , \_ -> Expect.equal errSnake (Err IllegalMove)
                    ]
                    ()
        ]


toList : Test
toList =
    describe "toList"
        [ test "for initial snake" <|
            \_ ->
                Expect.equal
                    (Snake.toList initialSnake)
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
                            |> Result.map Snake.toList
                in
                Expect.equal
                    segments
                    (Ok
                        [ ( 5, 3 ), ( 5, 2 ), ( 5, 1 ), ( 4, 1 ), ( 3, 1 ), ( 2, 1 ) ]
                    )
        ]
