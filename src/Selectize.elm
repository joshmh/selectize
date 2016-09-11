module Selectize
    exposing
        ( init
        , update
        , view
        , selectizeItem
        , selectedIds
        , Model
        , Msg
        , Item
        , HtmlOptions
        , HtmlClasses
        , focused
        , blurred
        )

import Html exposing (..)
import Html.Attributes exposing (value, defaultValue, maxlength, class, classList, id)
import Html.Events exposing (onInput, onBlur, onFocus, onMouseDown, onClick, on)
import Fuzzy
import String
import Json.Decode


-- MODEL


type alias HtmlOptions =
    { instructionsForBlank : String
    , noMatches : String
    , typeForMore : String
    , atMaxLength : String
    , noOptions : String
    , classes : HtmlClasses
    }


type alias HtmlClasses =
    { container : String
    , noOptions : String
    , singleItemContainer : String
    , multiItemContainer : String
    , selectBox : String
    , selectedItems : String
    , fallbackItems : String
    , fallbackItem : String
    , selectedItem : String
    , boxContainer : String
    , boxItems : String
    , boxItem : String
    , boxItemActive : String
    , info : String
    , infoNoMatches : String
    , inputEditing : String
    }


type alias H =
    HtmlOptions


type alias Item idType =
    { id : idType
    , selectedDisplay : String
    , optionDisplay : String
    , searchWords : List String
    }


type Status
    = Initial
    | Editing
    | Cleared
    | Idle
    | Blurred


selectizeItem : idType -> String -> String -> List String -> Item idType
selectizeItem id selectedDisplay optionDisplay searchWords =
    { id = id
    , selectedDisplay = selectedDisplay
    , optionDisplay = optionDisplay
    , searchWords = List.map clean searchWords
    }


type alias Items idType =
    List (Item idType)


type alias Config =
    {}


type alias Model idType =
    { maxItems : Int
    , boxLength : Int
    , selectedItems : Items idType
    , availableItems : Items idType
    , boxItems : Items idType
    , boxPosition : Int
    , status : Status
    }


pickItems : Items idType -> List idType -> Items idType
pickItems items ids =
    List.filter (\item -> (List.member item.id ids)) items


defaultItems : Int -> Items idType -> Items idType -> Items idType
defaultItems boxLength availableItems selectedItems =
    List.take boxLength (diffItems availableItems selectedItems)


init : Int -> Int -> List idType -> Items idType -> Model idType
init maxItems boxLength selectedIds availableItems =
    let
        selectedItems =
            pickItems availableItems (List.take maxItems selectedIds)
    in
        { maxItems = maxItems
        , boxLength = boxLength
        , selectedItems = selectedItems
        , availableItems = availableItems
        , boxItems = defaultItems boxLength availableItems selectedItems
        , boxPosition = 0
        , status = Blurred
        }



-- UPDATE


type Msg idType
    = Input String
    | KeyDown Int
    | KeyUp Int
    | MouseClick (Item idType)
    | Blur
    | Focus


focused : Msg idType -> Bool
focused msg =
    msg == Focus


blurred : Msg idType -> Bool
blurred msg =
    msg == Blur


selectedIds : Model idType -> List idType
selectedIds model =
    List.map .id model.selectedItems


clean : String -> String
clean s =
    String.trim s
        |> String.toLower


score : String -> Item idType -> ( Int, Item idType )
score needle hay =
    let
        cleanNeedle =
            clean needle

        match keyword =
            Fuzzy.match [] [] cleanNeedle keyword
                |> .score

        score =
            List.map match hay.searchWords
                |> List.minimum
                |> Maybe.withDefault
                    10000
    in
        ( score, hay )


diffItems : Items idType -> Items idType -> Items idType
diffItems a b =
    let
        isEqual itemA itemB =
            itemA.id == itemB.id

        notInB b item =
            (List.any (isEqual item) b)
                |> not
    in
        List.filter (notInB b) a


updateInput : String -> Model idType -> ( Model idType, Cmd (Msg idType) )
updateInput string model =
    if (String.length string == 0) then
        { model
            | status = Idle
            , boxItems =
                defaultItems model.boxLength model.availableItems (Debug.log "DEBUG1" model.selectedItems)
        }
            ! []
    else
        let
            unselectedItems =
                diffItems model.availableItems (Debug.log "DEBUG2" model.selectedItems)

            boxItems =
                List.map (score string) unselectedItems
                    |> List.sortBy fst
                    |> List.take model.boxLength
                    |> List.filter (((>) 1100) << fst)
                    |> List.map snd
        in
            { model | status = Editing, boxItems = boxItems } ! []


updateSelectedItem : Item idType -> Model idType -> ( Model idType, Cmd (Msg idType) )
updateSelectedItem item model =
    let
        selectedItems =
            model.selectedItems ++ [ item ]

        boxItems =
            defaultItems model.boxLength model.availableItems selectedItems
    in
        { model
            | status = Cleared
            , selectedItems = selectedItems
            , boxItems = boxItems
            , boxPosition = 0
        }
            ! []


updateEnterKey : Model idType -> ( Model idType, Cmd (Msg idType) )
updateEnterKey model =
    let
        maybeItem =
            (List.head << (List.drop model.boxPosition)) model.boxItems
    in
        case maybeItem of
            Nothing ->
                model ! []

            Just item ->
                updateSelectedItem item model


updateBox : Int -> Model idType -> ( Model idType, Cmd (Msg idType) )
updateBox keyCode model =
    if List.length model.selectedItems == model.maxItems then
        model ! []
    else
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

            _ ->
                model ! []


updateBoxInitial : Int -> Model idType -> ( Model idType, Cmd (Msg idType) )
updateBoxInitial keyCode originalModel =
    let
        ( model, cmd ) =
            updateBox keyCode originalModel
    in
        case keyCode of
            -- backspace
            8 ->
                let
                    allButLast =
                        max 0 ((List.length model.selectedItems) - 1)

                    newSelectedItems =
                        List.take allButLast model.selectedItems

                    boxItems =
                        defaultItems model.boxLength model.availableItems newSelectedItems
                in
                    { model
                        | selectedItems = newSelectedItems
                        , boxItems = boxItems
                    }
                        ! [ cmd ]

            _ ->
                model ! [ cmd ]


updateKey : Int -> Model idType -> ( Model idType, Cmd (Msg idType) )
updateKey keyCode model =
    case model.status of
        Editing ->
            updateBox keyCode model

        Initial ->
            updateBoxInitial keyCode model

        Idle ->
            updateBoxInitial keyCode model

        Cleared ->
            updateBoxInitial keyCode model

        Blurred ->
            model ! []


update : Msg idType -> Model idType -> ( Model idType, Cmd (Msg idType) )
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
            updateSelectedItem item model

        Blur ->
            { model
                | status = Blurred
                , boxPosition = 0
                , boxItems = defaultItems model.boxLength model.availableItems model.selectedItems
            }
                ! []

        Focus ->
            { model
                | status = Initial
                , boxPosition = 0
                , boxItems = defaultItems model.boxLength model.availableItems model.selectedItems
            }
                ! []



-- VIEW


itemView : HtmlOptions -> Bool -> Item idType -> Html (Msg idType)
itemView h isFallback item =
    span
        [ classList
            [ ( h.classes.selectedItem, True )
            , ( h.classes.fallbackItem, isFallback )
            ]
        ]
        [ text item.selectedDisplay ]


fallbackItemsView : HtmlOptions -> Items idType -> Items idType -> Model idType -> Html (Msg idType)
fallbackItemsView h fallbackItems selectedItems model =
    let
        classes =
            classList
                [ ( h.classes.selectedItems, True )
                , ( h.classes.fallbackItems, List.length selectedItems == 0 )
                ]

        isFallback =
            List.length selectedItems == 0

        items =
            if isFallback then
                fallbackItems
            else
                selectedItems
    in
        span [ classes ] (List.map (itemView h isFallback) items)


itemsView : HtmlOptions -> Items idType -> Items idType -> Model idType -> Html (Msg idType)
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


editingBoxView : HtmlOptions -> Model idType -> Html (Msg idType)
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
                [ text item.optionDisplay
                ]
    in
        div [ class c.boxItems ] (List.indexedMap boxItemHtml model.boxItems)


idleBoxView : HtmlOptions -> Model idType -> Html (Msg idType)
idleBoxView h model =
    let
        remainingItems =
            List.length model.availableItems - List.length model.selectedItems

        typeForMore =
            if remainingItems > model.boxLength then
                div [ class h.classes.info ] [ text h.typeForMore ]
            else
                span [] []
    in
        if List.length model.selectedItems == model.maxItems then
            span [] []
        else
            div [ class h.classes.boxContainer ]
                [ editingBoxView h model
                , typeForMore
                ]


noMatches : HtmlOptions -> Model idType -> Html (Msg idType)
noMatches h model =
    if List.length model.boxItems == 0 then
        div
            [ classList
                [ ( h.classes.info, True )
                , ( h.classes.infoNoMatches, True )
                ]
            ]
            [ text h.noMatches ]
    else
        span [] []


boxView : HtmlOptions -> Model idType -> Html (Msg idType)
boxView h model =
    case model.status of
        Editing ->
            div [ class h.classes.boxContainer ]
                [ editingBoxView h model
                , noMatches h model
                ]

        Initial ->
            idleBoxView h model

        Idle ->
            idleBoxView h model

        Cleared ->
            idleBoxView h model

        Blurred ->
            span [] []


view : HtmlOptions -> List idType -> Model idType -> Html (Msg idType)
view h fallbackIds model =
    if List.length model.availableItems == 0 then
        div [ class h.classes.container ]
            [ div [ class h.classes.noOptions ] [ text h.noOptions ] ]
    else
        let
            fallbackItems =
                pickItems model.availableItems fallbackIds

            editInput =
                case model.status of
                    Initial ->
                        if (List.length model.selectedItems) < model.maxItems then
                            input [ onBlur Blur, onInput Input ] []
                        else
                            input [ onBlur Blur, onInput Input, maxlength 0 ] []

                    Idle ->
                        if (List.length model.selectedItems) < model.maxItems then
                            input [ onBlur Blur, onInput Input ] []
                        else
                            input [ onBlur Blur, onInput Input, maxlength 0 ] []

                    Editing ->
                        let
                            maxlength' =
                                if List.length model.boxItems == 0 then
                                    0
                                else
                                    524288
                        in
                            input [ maxlength maxlength', onBlur Blur, onInput Input, class h.classes.inputEditing ] []

                    Cleared ->
                        input [ onKeyUp KeyUp, value "", onBlur Blur, onInput Input ] []

                    Blurred ->
                        input [ maxlength 0, onFocus Focus, value "" ] []
        in
            div [ class h.classes.container ]
                [ label
                    [ classList
                        [ ( h.classes.singleItemContainer, model.maxItems == 1 )
                        , ( h.classes.multiItemContainer, model.maxItems > 1 )
                        ]
                    ]
                    [ span [ class h.classes.selectBox, onKeyDown KeyDown ]
                        [ span [] [ itemsView h fallbackItems model.selectedItems model ]
                        , editInput
                        ]
                    , boxView h model
                    ]
                ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger Html.Events.keyCode)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger Html.Events.keyCode)
