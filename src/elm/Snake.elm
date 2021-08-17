module Snake exposing
    ( Direction(..)
    , IllegalMove(..)
    , Snake
    , foldSegments
    , head
    , initialize
    , mapSegments
    , move
    , toList
    )

import Set


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


defaultLength : Int
defaultLength =
    5


initialize : Position -> Maybe Int -> Snake
initialize position length =
    Snake
        { head = position
        , tail =
            List.repeat
                (length
                    |> Maybe.withDefault defaultLength
                )
                Left
        }


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
        newSnake : Snake
        newSnake =
            case snake.tail of
                first :: _ ->
                    if first == direction then
                        Snake snake

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
                        Snake { head = newHead, tail = newTail }

                _ ->
                    Snake snake
    in
    if snakeBitesItself newSnake then
        Err IllegalMove

    else
        Ok newSnake


snakeBitesItself : Snake -> Bool
snakeBitesItself =
    foldSegments
        (\position ( positions, acc ) ->
            if Set.member position positions then
                ( positions, True )

            else
                ( Set.insert position positions, acc )
        )
        ( Set.empty, False )
        >> Tuple.second


foldSegments : (Position -> b -> b) -> b -> Snake -> b
foldSegments f initialAcc (Snake snake) =
    List.foldl
        (\direction ( position, acc ) ->
            let
                newPosition =
                    step direction position
            in
            ( newPosition, f newPosition acc )
        )
        ( snake.head, f snake.head initialAcc )
        snake.tail
        |> Tuple.second


mapSegments : (Position -> a) -> Snake -> List a
mapSegments f =
    foldSegments (\position acc -> f position :: acc) []
        >> List.reverse


toList : Snake -> List Position
toList =
    mapSegments identity