module App exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text, program)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL

type alias Grid =
    Array (Array (Maybe (Int, Int)))


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


type Msg
    = NoOp



-- VIEW
renderCell: Int -> Int -> Svg (Msg)
renderCell i j =
    rect [ x (toString (i * 20)), y (toString ( j * 20)), width "20", height "20" ] []


renderGrid: List (Svg Msg)
renderGrid =
    List.range 0 20
        |> List.concatMap(\y ->
            List.range 0 20
                |> List.map (\x -> renderCell x y)
           )



view : Model -> Html Msg
view model =
    svg
        [ width "400", height "400", viewBox "0 0 400 400" ]
            renderGrid




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
