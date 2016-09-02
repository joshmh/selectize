module Selectize exposing (..)

import Html exposing (..)

-- MODEL

type alias AvailableItem =
    {code: String
    , display : String
    , searchWords : List String
    }

type alias SelectedItem =
    {code: String
    , display : String
    }

type alias Model =
    { selectedItems : List SelectedItem
    , availableItems : List AvailableItem
    ,currentInput: String
    }

init : List SelectedItem -> List AvailableItem -> (Model, Cmd Msg)
init selectedItems availableItems =
    {selectedItems = selectedItems, availableItems = availableItems,
    currentInput = ""} ! []


-- UPDATE

type Msg
    = Something

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
    Something -> model ! []

-- VIEW
itemView : SelectedItem -> Html Msg
itemView selectedItem =
    div [] [text selectedItem.display]

itemsView : Model -> Html Msg
itemsView model =
    div [] (List.map itemView model.selectedItems)

selectBoxView: Model -> Html Msg
selectBoxView model =
    div [] []

view : Model -> Html Msg
view model =
    div []
        [ div [] [itemsView model]
        , input [] []
        , (selectBoxView model)
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
