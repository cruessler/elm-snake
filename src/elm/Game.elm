module Game exposing
    ( Game
    , Square(..)
    , face
    , getBoard
    , getRound
    , initialize
    , isLost
    , isPaused
    , isRunning
    , mapSquares
    , pause
    , resume
    , step
    , withFruits
    )

import Random exposing (Seed)
import Random.Set
import Set exposing (Set)
import Snake exposing (Direction(..), Position, Snake)


type alias Options =
    { board : Maybe Board
    , probabilityForFruit : Float
    , initialSeed : Seed
    }


type alias State =
    { snake : Snake
    , fruits : Set Position
    , direction : Direction
    , board : Board
    , round : Int
    , probabilityForFruit : Float
    , seed : Seed
    }


type alias Board =
    { width : Int
    , height : Int
    }


type Game
    = Running State
    | Paused State
    | Lost
        { snake : Snake
        , fruits : Set Position
        , board : Board
        , probabilityForFruit : Float
        , seed : Seed
        }


type Square
    = Empty
    | PartOfSnake
    | Fruit


defaultInitialWidth : Int
defaultInitialWidth =
    40


defaultInitialHeight : Int
defaultInitialHeight =
    40


maximumInitialLength : Int
maximumInitialLength =
    5


maximumNumberOfFruits : Board -> Int
maximumNumberOfFruits board =
    board.width * board.height // 40


initialize : Options -> Game
initialize options =
    let
        board : Board
        board =
            options.board
                |> Maybe.withDefault
                    { width = defaultInitialWidth, height = defaultInitialHeight }

        initialLength : Int
        initialLength =
            min (board.width // 2) maximumInitialLength

        initialPosition : Position
        initialPosition =
            ( (board.width + initialLength - 1) // 2
            , (board.height - 1) // 2
            )
    in
    Running
        { snake = Snake.initialize initialPosition (Just initialLength)
        , fruits = Set.empty
        , direction = Right
        , board = board
        , round = 0
        , probabilityForFruit = options.probabilityForFruit
        , seed = options.initialSeed
        }


withFruits : Set Position -> Game -> Game
withFruits fruits game =
    let
        snakeSquares =
            getSnake game
                |> Snake.toList
                |> Set.fromList

        legalFruits =
            Set.diff fruits snakeSquares
    in
    case game of
        Running state ->
            Running { state | fruits = legalFruits }

        Paused state ->
            Paused { state | fruits = legalFruits }

        Lost state ->
            Lost { state | fruits = legalFruits }


pause : Game -> Game
pause game =
    case game of
        Running state ->
            Paused state

        _ ->
            game


resume : Game -> Game
resume game =
    case game of
        Paused state ->
            Running state

        _ ->
            game


randomFruit : State -> Seed -> ( Maybe Position, Seed )
randomFruit state seed =
    let
        generator : Random.Generator (Maybe Position)
        generator =
            Random.float 0 1
                |> Random.andThen
                    (\p ->
                        let
                            placeNewFruit =
                                p
                                    < state.probabilityForFruit
                                    && Set.size state.fruits
                                    < maximumNumberOfFruits state.board
                        in
                        if placeNewFruit then
                            Random.Set.sample (freeSquares state)

                        else
                            Random.constant Nothing
                    )
    in
    Random.step generator seed


freeSquares : State -> Set Position
freeSquares state =
    let
        snakeSquares =
            state.snake
                |> Snake.toList
                |> Set.fromList

        occupiedSquares =
            Set.union snakeSquares state.fruits
    in
    Set.diff (allSquares state) occupiedSquares


allSquares : State -> Set Position
allSquares state =
    let
        xs =
            List.range 0 (state.board.width - 1)

        ys =
            List.range 0 (state.board.height - 1)
    in
    ys
        |> List.concatMap
            (\y ->
                xs |> List.map (\x -> ( x, y ))
            )
        |> Set.fromList


step : Game -> Game
step game =
    case game of
        Running state ->
            let
                newHead =
                    Snake.peek state.direction state.snake

                eatsFruit =
                    Set.member newHead state.fruits

                f : Direction -> Snake -> Result Snake.IllegalMove Snake
                f =
                    if eatsFruit then
                        Snake.grow

                    else
                        Snake.move
            in
            case f state.direction state.snake of
                Ok newSnake ->
                    if snakeOffBoard state.board newSnake then
                        Lost
                            { snake = state.snake
                            , fruits = state.fruits
                            , board = state.board
                            , probabilityForFruit = state.probabilityForFruit
                            , seed = state.seed
                            }

                    else
                        let
                            newFruits =
                                Set.remove newHead state.fruits

                            newState =
                                { state
                                    | snake = newSnake
                                    , fruits = newFruits
                                    , round = state.round + 1
                                }
                                    |> maybePlaceNewFruit
                        in
                        Running newState

                Err _ ->
                    Lost
                        { snake = state.snake
                        , fruits = state.fruits
                        , board = state.board
                        , probabilityForFruit = state.probabilityForFruit
                        , seed = state.seed
                        }

        _ ->
            game


maybePlaceNewFruit : State -> State
maybePlaceNewFruit state =
    let
        ( newFruit, newSeed ) =
            randomFruit state state.seed

        newFruits =
            newFruit
                |> Maybe.map (\fruit -> Set.insert fruit state.fruits)
                |> Maybe.withDefault state.fruits
    in
    { state
        | fruits = newFruits
        , seed = newSeed
    }


snakeOffBoard : Board -> Snake -> Bool
snakeOffBoard board snake =
    snake
        |> Snake.anySegment
            (\( x, y ) ->
                x < 0 || x >= board.width || y < 0 || y >= board.height
            )


face : Direction -> Game -> Game
face direction game =
    case game of
        Running state ->
            if
                (state.snake
                    |> Snake.orientation
                    |> Maybe.map Snake.oppositeDirection
                )
                    /= Just direction
            then
                Running { state | direction = direction }

            else
                Running state

        _ ->
            game


isRunning : Game -> Bool
isRunning game =
    case game of
        Running _ ->
            True

        _ ->
            False


isPaused : Game -> Bool
isPaused game =
    case game of
        Paused _ ->
            True

        _ ->
            False


isLost : Game -> Bool
isLost game =
    case game of
        Lost _ ->
            True

        _ ->
            False


getRound : Game -> Maybe Int
getRound game =
    case game of
        Running state ->
            Just state.round

        _ ->
            Nothing


getBoard : Game -> Board
getBoard game =
    case game of
        Running state ->
            state.board

        Paused state ->
            state.board

        Lost state ->
            state.board


getSnake : Game -> Snake
getSnake game =
    case game of
        Running state ->
            state.snake

        Paused state ->
            state.snake

        Lost state ->
            state.snake


getFruits : Game -> Set Position
getFruits game =
    case game of
        Running state ->
            state.fruits

        Paused state ->
            state.fruits

        Lost state ->
            state.fruits


mapSquares : (Position -> Square -> a) -> Game -> List a
mapSquares f game =
    let
        board =
            getBoard game

        fruits =
            getFruits game

        xs =
            List.range 0 (board.width - 1)

        ys =
            List.range 0 (board.height - 1)

        snakePositions =
            getSnake game
                |> Snake.toList
                |> Set.fromList
    in
    ys
        |> List.concatMap
            (\y ->
                xs
                    |> List.map
                        (\x ->
                            let
                                position =
                                    ( x, y )

                                square =
                                    if Set.member position fruits then
                                        Fruit

                                    else if Set.member position snakePositions then
                                        PartOfSnake

                                    else
                                        Empty
                            in
                            f position square
                        )
            )
