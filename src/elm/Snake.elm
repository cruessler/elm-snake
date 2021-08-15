module Snake exposing (Direction(..), Snake, head, initialize, mapSegments, move)


type Direction
    = Up
    | Right
    | Down
    | Left


type IllegalMove
    = IllegalMove


type alias Position =
    ( Int, Int )


type Snake
    = Snake { head : Position, tail : List Direction }


initialLength : Int
initialLength =
    5


initialize : Position -> Int -> Snake
initialize position length =
    Snake { head = position, tail = List.repeat initialLength Left }


head : Snake -> Position
head (Snake snake) =
    snake.head


step : Direction -> ( Int, Int ) -> ( Int, Int )
step direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


reverse : Direction -> Direction
reverse direction =
    case direction of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


move : Direction -> Snake -> Result IllegalMove Snake
move direction (Snake snake) =
    let
        newSnake =
            case snake.tail of
                first :: _ ->
                    if first == direction then
                        snake

                    else
                        let
                            newHead =
                                step direction snake.head

                            newTail =
                                snake.tail
                                    |> List.reverse
                                    |> List.drop 1
                                    |> List.reverse
                                    |> (\tail -> reverse direction :: tail)
                        in
                        { head = newHead, tail = newTail }

                _ ->
                    snake
    in
    Ok (Snake newSnake)


mapSegments : (( Int, Int ) -> a) -> Snake -> List a
mapSegments f (Snake snake) =
    List.foldl
        (\direction ( position, acc ) ->
            let
                newPosition =
                    step direction position
            in
            ( newPosition, f newPosition :: acc )
        )
        ( snake.head, [ f snake.head ] )
        snake.tail
        |> Tuple.second
        |> List.reverse
