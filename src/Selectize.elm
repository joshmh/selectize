module Selectize
    exposing
        ( init
        , update
        , view
        , selectizeItem
        , Model
        , Msg
        , Item
        )

import Task
import Html exposing (..)
import Html.Attributes exposing (value, defaultValue, readonly, maxlength)
import Html.Events exposing (onInput, onBlur, onFocus, on)
import Fuzzy
import String
import Json.Decode


-- MODEL


type alias Item =
    { code : String
    , display : String
    , searchWords : List String
    }


type Status
    = Initial
    | Editing
    | Cleared
    | Blurred


selectizeItem : String -> String -> List String -> Item
selectizeItem code display searchWords =
    { code = code
    , display = display
    , searchWords = searchWords
    }


type alias Items =
    List Item


type alias Model =
    { maxItems : Int
    , boxLength : Int
    , selectedItems : List Item
    , availableItems : List Item
    , boxItems : List Item
    , boxPosition : Int
    , status : Status
    }


init : Int -> Int -> List Item -> List Item -> Model
init maxItems boxLength selectedItems availableItems =
    { maxItems = maxItems
    , boxLength = boxLength
    , selectedItems = selectedItems
    , availableItems = availableItems
    , boxItems = []
    , boxPosition = 0
    , status = Blurred
    }



-- UPDATE


type Msg
    = Input String
    | KeyDown Int
    | KeyUp Int
    | Blur
    | Focus


clean : String -> String
clean s =
    String.trim s
        |> String.toLower


score : String -> Item -> ( Int, Item )
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


diffItems : Items -> Items -> Items
diffItems a b =
    let
        isEqual itemA itemB =
            itemA.code == itemB.code

        notInB b item =
            (List.any (isEqual item) b)
                |> not
    in
        List.filter (notInB b) a


updateInput : String -> Model -> ( Model, Cmd Msg )
updateInput string model =
    if (String.length string < 2) then
        { model | status = Editing, boxItems = [] } ! []
    else
        let
            unselectedItems =
                diffItems model.availableItems model.selectedItems

            boxItems =
                List.map (score string) unselectedItems
                    |> List.sortBy fst
                    |> List.take model.boxLength
                    |> List.filter (((>) 1100) << fst)
                    |> List.map snd
        in
            { model | status = Editing, boxItems = boxItems } ! []


updateKey : Int -> Model -> ( Model, Cmd Msg )
updateKey keyCode model =
    case keyCode of
        -- up
        38 ->
            { model | boxPosition = (max 0 (model.boxPosition - 1)) } ! []

        -- down
        40 ->
            { model
                | boxPosition =
                    (min ((List.length model.boxItems) - 1)
                        (model.boxPosition + 1)
                    )
            }
                ! []

        -- enter
        13 ->
            let
                maybeItem =
                    (List.head << (List.drop model.boxPosition)) model.boxItems
            in
                case maybeItem of
                    Nothing ->
                        model ! []

                    Just item ->
                        { model
                            | status = Cleared
                            , selectedItems = model.selectedItems ++ [ item ]
                            , boxPosition = 0
                        }
                            ! []

        -- backspace
        8 ->
            if model.status == Initial then
                let
                    allButLast =
                        max 0 ((List.length model.selectedItems) - 1)

                    newSelectedItems =
                        List.take allButLast model.selectedItems
                in
                    { model | selectedItems = newSelectedItems } ! []
            else
                model ! []

        _ ->
            model ! []


dispatch : Msg -> Cmd Msg
dispatch msg =
    Task.perform identity identity (Task.succeed msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input string ->
            updateInput string model

        KeyDown code ->
            updateKey code model

        KeyUp code ->
            if model.status == Cleared && code == 13 then
                { model | status = Initial } ! []
            else
                model ! []

        Blur ->
            { model | status = Blurred, boxPosition = 0 } ! []

        Focus ->
            { model | status = Initial, boxPosition = 0 } ! []



-- VIEW


itemView : Item -> Html Msg
itemView item =
    div [] [ text item.display ]


itemsView : List Item -> Html Msg
itemsView items =
    div [] (List.map itemView items)


boxView : Model -> Html Msg
boxView model =
    let
        boxItemHtml pos item =
            if model.boxPosition == pos then
                div [] [ text ("* " ++ item.display) ]
            else
                div [] [ text item.display ]
    in
        case model.status of
            Editing ->
                div [] (List.indexedMap boxItemHtml model.boxItems)

            Initial ->
                if ((List.length model.selectedItems) == 0) then
                    div [] [ text "Start typing for options" ]
                else
                    span [] []

            _ ->
                span [] []


view : Model -> Html Msg
view model =
    let
        editInput =
            case (Debug.log "DEBUG1" model.status) of
                Initial ->
                    if (List.length model.selectedItems) < model.maxItems then
                        input [ onKeyDown KeyDown, onBlur Blur, onInput Input ] []
                    else
                        input [ maxlength 0, onKeyDown KeyDown ] []

                Editing ->
                    input [ onKeyDown KeyDown, onBlur Blur, onInput Input ] []

                Cleared ->
                    input [ onKeyUp KeyUp, value "", onBlur Blur, onInput Input ] []

                Blurred ->
                    input [ readonly True, onFocus Focus ] []
    in
        div []
            [ div []
                [ div [] [ itemsView model.selectedItems ]
                , editInput
                ]
            , boxView model
            ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger Html.Events.keyCode)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger Html.Events.keyCode)
