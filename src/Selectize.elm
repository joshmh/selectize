module Selectize
    exposing
        ( init
        , update
        , view
        , selectizeItem
        , Model
        , Msg
        , Item
        , keyDown
        , keyUp
        )

import Html exposing (..)
import Html.Attributes exposing (value, defaultValue)
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


type Status
    = Idle
    | Editing
    | Initial


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
    { selectedItems : Items x
    , availableItems : Items x
    , boxItems : Items x
    , boxLength : Int
    , boxPosition : Int
    , boxShow : Bool
    , status : Status
    , inputText : String
    }


init : Items x -> Items x -> ( Model x, Cmd Msg )
init initialItems availableItems =
    { availableItems = availableItems
    , selectedItems = initialItems
    , boxItems = []
    , boxLength = 5
    , boxPosition = 0
    , boxShow = False
    , status = Initial
    , inputText = ""
    }
        ! []



-- UPDATE


type Msg
    = Input String
    | KeyDown Int
    | KeyUp Int


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


updateInput : String -> Model x -> ( Model x, Cmd Msg )
updateInput string model =
    if (String.length string < 2) then
        { model | status = Editing, boxItems = [], inputText = string } ! []
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
            { model | status = Editing, inputText = string, boxItems = boxItems } ! []


updateKey : Int -> Model x -> ( Model x, Cmd Msg )
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
                            | status = Idle
                            , selectedItems = model.selectedItems ++ [ item ]
                            , inputText = ""
                        }
                            ! []

        _ ->
            model ! []


update : Msg -> Model x -> ( Model x, Cmd Msg )
update msg model =
    case msg of
        Input string ->
            updateInput string model

        KeyDown code ->
            updateKey code model

        KeyUp code ->
            if model.status == Idle && code == 13 then
                { model | status = Editing } ! []
            else
                model ! []



-- VIEW


itemView : Item x -> Html Msg
itemView item =
    div [] [ text item.display ]


itemsView : List (Item x) -> Html Msg
itemsView items =
    div [] (List.map itemView items)


boxView : Int -> Items x -> Html Msg
boxView boxPosition boxItems =
    let
        boxItemHtml pos item =
            if boxPosition == pos then
                div [] [ text ("* " ++ item.display) ]
            else
                div [] [ text item.display ]
    in
        div [] (List.indexedMap boxItemHtml boxItems)


view : Model x -> Html Msg
view model =
    let
        editInput =
            case model.status of
                Initial ->
                    input [ onInput Input ] []

                Editing ->
                    input [ onInput Input ] []

                Idle ->
                    input [ value "", onInput Input ] []
    in
        div []
            [ div []
                [ div [] [ itemsView (Debug.log "DEBUG1" model.selectedItems) ]
                , editInput
                ]
            , boxView model.boxPosition model.boxItems
            ]


keyUp : Int -> Msg
keyUp code =
    KeyUp code


keyDown : Int -> Msg
keyDown code =
    KeyDown code
