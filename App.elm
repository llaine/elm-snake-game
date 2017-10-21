module App exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text, program)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import Keyboard


-- MODEL

type Direction = Up | Down | Left | Right
type alias Position = { x: Int, y: Int }
type alias Model = { snakeBody: List Position, direction: Direction }


initialModel: Model
initialModel =
    Model [ Position 8 8 ] Left


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- MESSAGES


type Msg =
    NoOp
    | KeyMsg Keyboard.KeyCode


cellIsBody: Position -> Int -> Int -> Bool
cellIsBody position i j =
    if { x = i, y = j } == position then
        True
    else if { x = i - 1, y = j } == position then
        True
    else if { x = i - 2, y = j } == position then
        True
    else if { x = i - 3, y = j } == position then
        True
    else
        False


-- VIEW
renderCell: Model -> Int -> Int -> Svg (Msg)
renderCell model i j =
    let
        initialPosition=
            case head model.snakeBody of
                Just value ->
                    value
                Nothing ->
                    Position 0 0

        fillColor=
            if cellIsBody initialPosition i j then
                "red"
            else
                "black"
    in
        rect [
              x (toString (i * 20))
            , y (toString ( j * 20))
            , width "20"
            , height "20"
            , fill fillColor
            , stroke "white"
            , strokeWidth "1" ] []


renderGrid: Model -> List (Svg Msg)
renderGrid model =
    List.range 0 20
        |> List.concatMap(\y ->
            List.range 0 20
                |> List.map (\x -> renderCell model x y)
           )



view : Model -> Html Msg
view model =
    let
        grid = renderGrid model
    in
    svg
        [ width "400", height "400", viewBox "0 0 400 400" ]
            grid




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            let
                bodyUpdated=model.snakeBody -- TODO
            in
                ( Model model.snakeBody Down , Cmd.none )
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
        Sub.batch
            [ Keyboard.downs KeyMsg]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
