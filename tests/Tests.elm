module Tests exposing (game, grow, illegalMovements, move, toList)

import Expect
import Fuzz exposing (int)
import Game exposing (Game, Square(..))
import Random
import Set
import Snake exposing (Direction(..), IllegalMove(..), Snake)
import Test exposing (Test, describe, fuzz, test)


initialSnake : Snake
initialSnake =
    Snake.initialize ( 5, 1 ) Nothing


initialShortSnake : Snake
initialShortSnake =
    Snake.initialize ( 5, 1 ) (Just 1)


move : Test
move =
    describe "legal movements via move"
        [ test "moving back has no effect" <|
            \_ ->
                let
                    movedSnake =
                        Snake.move Left initialSnake
                in
                Expect.equal movedSnake (Ok initialSnake)
        , test "moving in any other direction moves the snake" <|
            \_ ->
                let
                    otherDirections =
                        [ ( Up, [ ( 5, 0 ), ( 5, 1 ) ] )
                        , ( Right, [ ( 6, 1 ), ( 5, 1 ) ] )
                        , ( Down, [ ( 5, 2 ), ( 5, 1 ) ] )
                        ]

                    expectations =
                        otherDirections
                            |> List.map
                                (\( direction, expectedList ) ->
                                    Snake.move direction
                                        >> Result.map Snake.toList
                                        >> Expect.equal (Ok expectedList)
                                )
                in
                Expect.all expectations initialShortSnake
        ]


grow : Test
grow =
    describe "legal movements via grow"
        [ test "moving back has no effect" <|
            \_ ->
                let
                    movedSnake =
                        Snake.move Left initialSnake
                in
                Expect.equal movedSnake (Ok initialSnake)
        , test "moving in any other direction grows the snake" <|
            \_ ->
                let
                    otherDirections =
                        [ ( Up, [ ( 5, 0 ), ( 5, 1 ), ( 4, 1 ) ] )
                        , ( Right, [ ( 6, 1 ), ( 5, 1 ), ( 4, 1 ) ] )
                        , ( Down, [ ( 5, 2 ), ( 5, 1 ), ( 4, 1 ) ] )
                        ]

                    expectations =
                        otherDirections
                            |> List.map
                                (\( direction, expectedList ) ->
                                    Snake.grow direction
                                        >> Result.map Snake.toList
                                        >> Expect.equal (Ok expectedList)
                                )
                in
                Expect.all expectations initialShortSnake
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


initialTinyGame : Game
initialTinyGame =
    Game.initialize
        { board = Just { width = 8, height = 3 }
        , initialSeed = Random.initialSeed 0
        , probabilityForFruit = 0.0
        }


game : Test
game =
    describe "Game"
        [ describe "isRunning"
            [ test "initial game" <|
                \_ ->
                    Expect.equal True
                        (Game.isRunning initialTinyGame)
            , test "paused game" <|
                \_ ->
                    Expect.equal False
                        (initialTinyGame |> Game.pause |> Game.isRunning)
            ]
        , describe "isPaused"
            [ test "initial game" <|
                \_ ->
                    Expect.equal False
                        (initialTinyGame |> Game.isPaused)
            , test "paused game" <|
                \_ ->
                    Expect.equal True
                        (initialTinyGame |> Game.pause |> Game.isPaused)
            ]
        , describe "step"
            [ test "running off the board loses the game" <|
                \_ ->
                    let
                        lostGame : Game
                        lostGame =
                            initialTinyGame
                                |> Game.step
                                |> Game.step
                                |> Game.step
                    in
                    Expect.equal True (Game.isLost lostGame)
            , test "not running off the board does not lose the game" <|
                \_ ->
                    let
                        lostGame : Game
                        lostGame =
                            initialTinyGame
                                |> Game.step
                    in
                    Expect.equal False (Game.isLost lostGame)
            , test "changing direction and not running off the board does not lose the game" <|
                \_ ->
                    let
                        lostGame : Game
                        lostGame =
                            initialTinyGame
                                |> Game.face Up
                                |> Game.step
                    in
                    Expect.equal False (Game.isLost lostGame)
            , test "changing direction and running off the board loses the game" <|
                \_ ->
                    let
                        lostGame : Game
                        lostGame =
                            initialTinyGame
                                |> Game.face Up
                                |> Game.step
                                |> Game.step
                    in
                    Expect.equal True (Game.isLost lostGame)
            , test "running into itself loses the game" <|
                \_ ->
                    let
                        lostGame : Game
                        lostGame =
                            initialTinyGame
                                |> Game.face Up
                                |> Game.step
                                |> Game.face Left
                                |> Game.step
                                |> Game.face Down
                                |> Game.step
                    in
                    Expect.equal True (Game.isLost lostGame)
            ]
        , fuzz int "no fruit should be where the snake is" <|
            \seed ->
                let
                    gameWithGuaranteedFruit =
                        Game.initialize
                            { board = Just { width = 5, height = 8 }
                            , initialSeed = Random.initialSeed seed
                            , probabilityForFruit = 1.0
                            }
                            |> Game.withFruits (Set.fromList [ ( 4, 3 ) ])

                    numberOfSnakeSquares =
                        gameWithGuaranteedFruit
                            |> Game.step
                            |> Game.mapSquares (\_ square -> square)
                            |> List.foldl
                                (\square acc ->
                                    if square == PartOfSnake then
                                        acc + 1

                                    else
                                        acc
                                )
                                0
                in
                {- The initial snake has 3 squares and the test is set up so
                   that the snake eats a fruit on its first move and then has 4
                   squares. This test makes sure the snake is never covered by
                   a fruit.
                -}
                Expect.equal 4 numberOfSnakeSquares
        ]
