module Selectize
    exposing
        ( init
        , update
        , view
        , selectizeItem
        , Model
        , Msg
        , Item
        , HtmlOptions
        , HtmlClasses
        )

import Task
import Html exposing (..)
import Html.Attributes exposing (value, defaultValue, readonly, maxlength, class, classList)
import Html.Events exposing (onInput, onBlur, onFocus, on)
import Fuzzy
import String
import Json.Decode


-- MODEL


type alias HtmlOptions =
    { instructionsForBlank : String
    , classes : HtmlClasses
    }


type alias HtmlClasses =
    { container : String
    , selectedItems : String
    , selectedItem : String
    , boxItems : String
    , boxItem : String
    , boxItemActive : String
    , instructionsForBlank : String
    }


type alias H =
    HtmlOptions


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
    if (String.length string == 0) then
        { model | status = Initial, boxItems = [] } ! []
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
    case model.status of
        Editing ->
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

                _ ->
                    model ! []

        Initial ->
            case keyCode of
                -- backspace
                8 ->
                    let
                        allButLast =
                            max 0 ((List.length model.selectedItems) - 1)

                        newSelectedItems =
                            List.take allButLast model.selectedItems
                    in
                        { model | selectedItems = newSelectedItems } ! []

                _ ->
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


itemView : HtmlOptions -> Item -> Html Msg
itemView h item =
    div [ class h.classes.selectedItem ] [ text item.code ]


itemsView : HtmlOptions -> List Item -> Html Msg
itemsView h items =
    div [ class h.classes.selectedItems ] (List.map (itemView h) items)


boxView : HtmlOptions -> Model -> Html Msg
boxView h model =
    case model.status of
        Editing ->
            let
                c =
                    h.classes

                boxItemHtml pos item =
                    div
                        [ classList
                            [ ( c.boxItem, True )
                            , ( c.boxItemActive, model.boxPosition == pos )
                            ]
                        ]
                        [ text item.display ]
            in
                div [ class c.boxItems ] (List.indexedMap boxItemHtml model.boxItems)

        Initial ->
            if List.length model.selectedItems == model.maxItems then
                span [] []
            else
                div [ class h.classes.instructionsForBlank ] [ text h.instructionsForBlank ]

        _ ->
            span [] []


view : HtmlOptions -> Model -> Html Msg
view h model =
    let
        editInput =
            case model.status of
                Initial ->
                    if (List.length model.selectedItems) < model.maxItems then
                        input [ onBlur Blur, onInput Input ] []
                    else
                        input [ readonly True ] []

                Editing ->
                    input [ onBlur Blur, onInput Input ] []

                Cleared ->
                    input [ onKeyUp KeyUp, value "", onBlur Blur, onInput Input ] []

                Blurred ->
                    input [ readonly True, onFocus Focus ] []
    in
        div [ class h.classes.container ]
            [ div [ onKeyDown KeyDown ]
                [ div [] [ itemsView h model.selectedItems ]
                , editInput
                ]
            , boxView h model
            ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger Html.Events.keyCode)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger Html.Events.keyCode)
