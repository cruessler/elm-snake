module Game exposing (Game, initialize, isLost, isPaused, isRunning, move, pause, resume, step)

import Snake exposing (Direction(..), Position, Snake)


type alias State =
    { snake : Snake
    , direction : Direction
    , board : Board
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
                        Running { state | snake = newSnake }

                Err _ ->
                    Lost { snake = state.snake, board = state.board }

        _ ->
            game


snakeOffBoard : Board -> Snake -> Bool
snakeOffBoard board snake =
    snake
        |> Snake.anySegment
            (\( x, y ) ->
                x < 0 || x >= board.width || y < 0 || y >= board.height
            )


move : Direction -> Game -> Game
move orientation game =
    case game of
        Running state ->
            if
                (state.snake
                    |> Snake.orientation
                    |> Maybe.map Snake.oppositeDirection
                )
                    /= Just orientation
            then
                Running { state | direction = orientation }

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
