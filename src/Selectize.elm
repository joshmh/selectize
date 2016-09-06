module Selectize
    exposing
        ( init
        , update
        , view
        , selectizeItem
        , selectedItemCodes
        , Model
        , Msg
        , Item
        , HtmlOptions
        , HtmlClasses
        , focused
        , blurred
        )

import Task
import Html exposing (..)
import Html.Attributes exposing (value, defaultValue, maxlength, class, classList)
import Html.Events exposing (onInput, onBlur, onFocus, onMouseDown, on)
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
    , singleItemContainer : String
    , selectBox : String
    , selectedItems : String
    , fallbackItems : String
    , selectedItem : String
    , boxItems : String
    , boxItem : String
    , boxItemActive : String
    , instructionsForBlank : String
    , inputEditing : String
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
    | Idle
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


pickItems : Items -> List String -> Items
pickItems items codes =
    List.filter (\item -> (List.member item.code codes)) items


init : Int -> Int -> List String -> Items -> Model
init maxItems boxLength selectedCodes availableItems =
    { maxItems = maxItems
    , boxLength = boxLength
    , selectedItems = pickItems availableItems selectedCodes
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
    | MouseClick Item
    | Blur
    | Focus


focused : Msg -> Bool
focused msg =
    msg == Focus


blurred : Msg -> Bool
blurred msg =
    msg == Blur


selectedItemCodes : Model -> List String
selectedItemCodes model =
    List.map .code model.selectedItems


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
        { model | status = Idle, boxItems = [] } ! []
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


updateMouse : Item -> Model -> ( Model, Cmd Msg )
updateMouse item model =
    { model
        | status = Cleared
        , selectedItems = model.selectedItems ++ [ item ]
        , boxPosition = 0
    }
        ! []


updateEnterKey : Model -> ( Model, Cmd Msg )
updateEnterKey model =
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
                    updateEnterKey model

                -- tab
                9 ->
                    updateEnterKey model

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

        Idle ->
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
                { model | status = Idle } ! []
            else
                model ! []

        MouseClick item ->
            updateMouse item model

        Blur ->
            { model | status = Blurred, boxPosition = 0 } ! []

        Focus ->
            { model | status = Initial, boxPosition = 0 } ! []



-- VIEW


itemView : HtmlOptions -> Item -> Html Msg
itemView h item =
    div [ class h.classes.selectedItem ] [ text item.code ]


fallbackItemsView : HtmlOptions -> List Item -> List Item -> Model -> Html Msg
fallbackItemsView h fallbackItems selectedItems model =
    let
        classes =
            classList
                [ ( h.classes.selectedItems, True )
                , ( h.classes.fallbackItems, List.length selectedItems == 0 )
                ]

        items =
            if List.length selectedItems == 0 then
                fallbackItems
            else
                selectedItems
    in
        div [ classes ] (List.map (itemView h) items)


itemsView : HtmlOptions -> List Item -> List Item -> Model -> Html Msg
itemsView h fallbackItems selectedItems model =
    case model.status of
        Editing ->
            fallbackItemsView h [] selectedItems model

        Initial ->
            fallbackItemsView h fallbackItems selectedItems model

        Idle ->
            fallbackItemsView h fallbackItems selectedItems model

        Cleared ->
            fallbackItemsView h fallbackItems selectedItems model

        Blurred ->
            fallbackItemsView h fallbackItems selectedItems model


editingBoxView : HtmlOptions -> Model -> Html Msg
editingBoxView h model =
    let
        c =
            h.classes

        boxItemHtml pos item =
            div
                [ classList
                    [ ( c.boxItem, True )
                    , ( c.boxItemActive, model.boxPosition == pos )
                    ]
                , onMouseDown (MouseClick item)
                ]
                [ text item.display ]
    in
        div [ class c.boxItems ] (List.indexedMap boxItemHtml model.boxItems)


idleBoxView : HtmlOptions -> Model -> Html Msg
idleBoxView h model =
    if model.maxItems == 1 then
        editingBoxView h model
    else if List.length model.selectedItems == model.maxItems then
        span [] []
    else
        div [ class h.classes.instructionsForBlank ] [ text h.instructionsForBlank ]


boxView : HtmlOptions -> Model -> Html Msg
boxView h model =
    case (Debug.log "DEBUG1" model.status) of
        Editing ->
            editingBoxView h model

        Initial ->
            idleBoxView h model

        Idle ->
            idleBoxView h model

        Cleared ->
            idleBoxView h model

        Blurred ->
            span [] []


view : HtmlOptions -> List String -> Model -> Html Msg
view h fallbackCodes model =
    let
        fallbackItems =
            pickItems model.availableItems fallbackCodes

        editInput =
            case model.status of
                Initial ->
                    if (List.length model.selectedItems) < model.maxItems then
                        input [ onBlur Blur, onInput Input ] []
                    else
                        input [ onBlur Blur, maxlength 0 ] []

                Idle ->
                    if (List.length model.selectedItems) < model.maxItems then
                        input [ onBlur Blur, onInput Input ] []
                    else
                        input [ onBlur Blur, maxlength 0 ] []

                Editing ->
                    input [ onBlur Blur, onInput Input, class h.classes.inputEditing ] []

                Cleared ->
                    input [ onKeyUp KeyUp, value "", onBlur Blur, onInput Input ] []

                Blurred ->
                    input [ maxlength 0, onFocus Focus, value "" ] []
    in
        div
            [ classList
                [ ( h.classes.container, True )
                , ( h.classes.singleItemContainer, model.maxItems == 1 )
                ]
            ]
            [ div [ class h.classes.selectBox, onKeyDown KeyDown ]
                [ div [] [ itemsView h fallbackItems model.selectedItems model ]
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
