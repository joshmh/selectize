module Selectize exposing (init, update, view, selectizeItem, Model, Msg, Item)

import Html exposing (..)
import Html.Events exposing (onInput)
import Fuzzy
import String


-- MODEL


type alias Item x =
    { itemType : x
    , code : String
    , display : String
    , searchWords : List String
    }


selectizeItem : x -> String -> String -> List String -> Item x
selectizeItem itemType code display searchWords =
    { itemType = itemType
    , code = code
    , display = display
    , searchWords = searchWords
    }


type alias Items x =
    List (Item x)


type alias Model x =
    { initialItems : Items x
    , selectedItems : Maybe (Items x)
    , availableItems : Items x
    , boxItems : List ( Int, Item x )
    }


init : Items x -> Items x -> ( Model x, Cmd Msg )
init initialItems availableItems =
    { initialItems = initialItems
    , availableItems = availableItems
    , selectedItems = Just initialItems
    , boxItems = []
    }
        ! []



-- UPDATE


type Msg
    = Input String


clean : String -> String
clean s =
    String.trim s
        |> String.toLower


score : String -> Item x -> ( Int, Item x )
score needle hay =
    let
        cleanNeedle =
            clean needle

        codeScore =
            Fuzzy.match [] [] cleanNeedle (clean hay.code)

        displayScore =
            Fuzzy.match [] [ " " ] cleanNeedle (clean hay.display)
    in
        ( min codeScore.score displayScore.score, hay )


diffItems : Items x -> Items x -> Items x
diffItems a b =
    let
        isEqual itemA itemB =
            itemA.itemType == itemB.itemType

        notInB b item =
            (List.any (isEqual item) b)
                |> not
    in
        List.filter (notInB b) a


update : Msg -> Model x -> ( Model x, Cmd Msg )
update msg model =
    case msg of
        Input string ->
            if (String.length string < 2) then
                { model | boxItems = [] } ! []
            else
                let
                    unselectedItems =
                        case model.selectedItems of
                            Nothing ->
                                model.availableItems

                            Just selectedItems ->
                                diffItems model.availableItems selectedItems

                    boxItems =
                        List.map (score string) unselectedItems
                            |> List.sortBy fst
                in
                    { model | boxItems = boxItems } ! []



-- VIEW


itemView : Item x -> Html Msg
itemView item =
    div [] [ text item.display ]


itemsView : List (Item x) -> Html Msg
itemsView items =
    div [] (List.map itemView items)


boxView : List ( Int, Item x ) -> Html Msg
boxView boxItems =
    let
        filtered =
            List.take 5 (Debug.log "DEBUG1" boxItems)
                |> List.filter (((>) 1100) << fst)
                |> List.map snd

        boxItemHtml item =
            div [] [ text item.display ]
    in
        div [] (List.map boxItemHtml filtered)


view : Model x -> Html Msg
view model =
    let
        selectedItems =
            Maybe.withDefault [] model.selectedItems
    in
        div []
            [ div []
                [ div [] [ itemsView selectedItems ]
                , input [ onInput Input ] []
                ]
            , boxView model.boxItems
            ]
