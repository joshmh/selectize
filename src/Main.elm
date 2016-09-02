module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Selectize exposing (..)

main : Program Never
main =
    App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
    {
    }

init : (Model, Cmd Msg)
init =
    {} ! []


-- UPDATE

type Msg
    = Something

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
    Something -> model ! []

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
