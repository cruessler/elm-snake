module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Game exposing (Game, Square(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode
import Random
import Snake exposing (Direction(..))
import Task
import Time


type alias Flags =
    Int


type alias Model =
    Game


type GameMsg
    = Face Direction
    | TogglePause
    | Restart


type Msg
    = NewGame Time.Posix
    | KeyPress String
    | Tick


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initialWidth : Int
initialWidth =
    30


initialGame : Int -> Game
initialGame seed =
    let
        options =
            { board =
                Just
                    { width = initialWidth
                    , height = initialWidth
                    }
            , probabilityForFruit = 0.05
            , initialSeed = Random.initialSeed seed
            }
    in
    Game.initialize options


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialGame flags, Cmd.none )


keybindings : Dict String GameMsg
keybindings =
    [ ( "e", Face Up )
    , ( "f", Face Right )
    , ( "d", Face Down )
    , ( "s", Face Left )
    , ( "p", TogglePause )
    , ( "j", Restart )
    ]
        |> Dict.fromList


updateGame : GameMsg -> Model -> ( Model, Cmd Msg )
updateGame msg model =
    let
        f : Game -> Game
        f =
            case msg of
                Face direction ->
                    Game.face direction

                TogglePause ->
                    if Game.isRunning model then
                        Game.pause

                    else
                        Game.resume

                Restart ->
                    identity

        cmd : Cmd Msg
        cmd =
            case msg of
                Restart ->
                    Task.perform NewGame Time.now

                _ ->
                    Cmd.none

        newGame =
            f model
    in
    ( newGame, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame now ->
            let
                newGame =
                    initialGame (Time.posixToMillis now)
            in
            ( newGame, Cmd.none )

        Tick ->
            let
                newGame =
                    Game.step model
            in
            ( newGame, Cmd.none )

        KeyPress key ->
            case Dict.get key keybindings of
                Just gameMsg ->
                    updateGame gameMsg model

                Nothing ->
                    ( model, Cmd.none )


msPerFrame : Float
msPerFrame =
    1000.0 / 60.0


{-| The interval between ticks.

This function returns values between `3.0 * msPerFrame` and `15.0 *
msPerFrame`.

-}
interval : Int -> Float
interval round =
    (msPerFrame * 15.0) - min (msPerFrame * 12.0) (round * 2 |> toFloat)


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        downs =
            Browser.Events.onKeyDown (Decode.map KeyPress keyDecoder)
    in
    Game.getRound model
        |> Maybe.map
            (\round ->
                Sub.batch
                    [ Time.every (interval round) (always Tick)
                    , downs
                    ]
            )
        |> Maybe.withDefault downs


board : Game -> List (Html Msg)
board game =
    Game.mapSquares
        (\_ square ->
            H.div
                [ [ ( "square", True )
                  , ( "snake", square == PartOfSnake )
                  , ( "fruit", square == Fruit )
                  ]
                    |> A.classList
                ]
                []
        )
        game


grid : List (H.Attribute Msg) -> List (Html Msg) -> Html Msg
grid attributes children =
    H.div
        (A.id "board" :: attributes)
        children


view : Model -> Html Msg
view model =
    let
        attributes =
            [ [ ( "lost", Game.isLost model )
              , ( "paused", Game.isPaused model )
              ]
                |> A.classList
            ]

        board_ =
            board model
                |> grid attributes
    in
    H.main_
        [ A.id "viewport" ]
        [ board_ ]
