module App exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text, program)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import Keyboard exposing (..)
import Char exposing (..)
import Time exposing (..)

-- MODEL


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    { x : Int, y : Int }


type alias Model =
    { head : Position
    , direction : Direction
    , body : List Position
    , eatablePositions : List Position
    }


initialModel : Model
initialModel =
    let
        initialPosition =
            Position 8 8

        eatablePositions =
            [ Position 0 0, Position 10 5, Position 1 5 ]
    in
        Model initialPosition Left [] eatablePositions


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- MESSAGES

type Msg
    = NoOp
    | KeyMsg Char
    | Tick Time


cellIsWritableElement : Position -> Int -> Int -> Bool
cellIsWritableElement position i j =
    if { x = i, y = j } == position then
        True
    else
        False



-- VIEW


renderRect : Int -> Int -> String -> Svg Msg
renderRect i j color =
    rect
        [ x (toString (i * 20))
        , y (toString (j * 20))
        , width "20"
        , height "20"
        , fill color
        , stroke "white"
        , strokeWidth "1"
        ]
        []


renderCell : Model -> Int -> Int -> Svg Msg
renderCell model i j =
    let
        initialPosition =
            model.head

        eatablePositionsList =
            List.map (\p -> ( p.x, p.y )) model.eatablePositions

        fillColor =
            if
                (cellIsWritableElement initialPosition i j)
                    || (List.member ( i, j ) eatablePositionsList)
            then
                "red"
            else
                "black"
    in
        renderRect i j fillColor


renderGrid : Model -> List (Svg Msg)
renderGrid model =
    List.range 0 20
        |> List.concatMap
            (\y ->
                List.range 0 20
                    |> List.map (\x -> renderCell model x y)
            )


view : Model -> Html Msg
view model =
    let
        grid =
            renderGrid model
    in
        svg
            [ width "400", height "400", viewBox "0 0 400 400" ]
            grid


updatePosition : Direction -> Position -> Position
updatePosition direction position =
    let
        (x, y) =
            case direction of
                Up ->
                    (position.x, position.y + 1)
                Down ->
                    (position.x, position.y - 1)
                Left ->
                    (position.x - 1, position.y)
                Right ->
                    (position.x + 1, position.y)
    in
        Position x y


directionFromLetter : Char -> Direction
directionFromLetter letter =
    case letter of
        'k' ->
            Down

        'j' ->
            Up

        'h' ->
            Left

        'l' ->
            Right

        _ ->
            Up



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            let
                nextPosition =
                    directionFromLetter code
            in
                ( Model model.head nextPosition model.body model.eatablePositions, Cmd.none )

        Tick time ->
            let
                snakeHead =
                    updatePosition model.direction model.head
            in
                ( Model snakeHead model.direction model.body model.eatablePositions, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses (\code -> KeyMsg (fromCode code))
        , Time.every second Tick
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
