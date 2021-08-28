module Game exposing
    ( Game
    , Square(..)
    , board
    , face
    , initialize
    , isLost
    , isPaused
    , isRunning
    , mapSquares
    , pause
    , resume
    , round
    , step
    )

import Set
import Snake exposing (Direction(..), Position, Snake)


type alias State =
    { snake : Snake
    , direction : Direction
    , board : Board
    , round : Int
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
        , board : Board
        }


type Square
    = Empty
    | PartOfSnake


defaultInitialWidth : Int
defaultInitialWidth =
    40


defaultInitialHeight : Int
defaultInitialHeight =
    40


initialLength : Int
initialLength =
    5


initialize : Maybe Board -> Game
initialize initialBoard =
    let
        initialBoard_ : Board
        initialBoard_ =
            initialBoard
                |> Maybe.withDefault
                    { width = defaultInitialWidth, height = defaultInitialHeight }

        initialPosition : Position
        initialPosition =
            ( (initialBoard_.width + initialLength - 1) // 2
            , (initialBoard_.height - 1) // 2
            )
    in
    Running
        { snake = Snake.initialize initialPosition (Just initialLength)
        , direction = Right
        , board = initialBoard_
        , round = 0
        }


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


step : Game -> Game
step game =
    case game of
        Running state ->
            case Snake.move state.direction state.snake of
                Ok newSnake ->
                    if snakeOffBoard state.board newSnake then
                        Lost { snake = state.snake, board = state.board }

                    else
                        Running
                            { state
                                | snake = newSnake
                                , round = state.round + 1
                            }

                Err _ ->
                    Lost { snake = state.snake, board = state.board }

        _ ->
            game


snakeOffBoard : Board -> Snake -> Bool
snakeOffBoard board_ snake_ =
    snake_
        |> Snake.anySegment
            (\( x, y ) ->
                x < 0 || x >= board_.width || y < 0 || y >= board_.height
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


round : Game -> Maybe Int
round game =
    case game of
        Running state ->
            Just state.round

        _ ->
            Nothing


board : Game -> Board
board game =
    case game of
        Running state ->
            state.board

        Paused state ->
            state.board

        Lost state ->
            state.board


snake : Game -> Snake
snake game =
    case game of
        Running state ->
            state.snake

        Paused state ->
            state.snake

        Lost state ->
            state.snake


mapSquares : (Position -> Square -> a) -> Game -> List a
mapSquares f game =
    let
        board_ =
            board game

        xs =
            List.range 0 (board_.width - 1)

        ys =
            List.range 0 (board_.height - 1)

        snakePositions =
            snake game
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
                                    if Set.member position snakePositions then
                                        PartOfSnake

                                    else
                                        Empty
                            in
                            f position square
                        )
            )
