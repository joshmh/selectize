module Selectize
    exposing
        ( initialSelectize
        , view
        , State
        , Config
        )

import Html exposing (..)
import Html.Attributes exposing (value, defaultValue, maxlength, class, classList, id)
import Html.Events exposing (on)
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


type Status
    = Initial
    | Editing
    | Cleared
    | Idle
    | Blurred


type alias State =
    { boxPosition : Int
    , status : Status
    }


type alias Config msg idType itemType =
    { maxItems : Int
    , boxLength : Int
    , toMsg : State -> msg
    , onAdd : idType -> msg
    , onRemove : msg
    , onFocus : msg
    , onBlur : msg
    , toId : itemType -> idType
    , toDisplay : itemType -> String
    , match : String -> List itemType -> List itemType
    , htmlOptions : HtmlOptions
    }


type alias Items itemType =
    { selectedItems : List itemType
    , availableItems : List itemType
    , boxItems : List itemType
    }


initialSelectized : State
initialSelectized =
    { boxPosition = -1, state = Blurred }



-- UPDATE


clean : String -> String
clean s =
    String.trim s
        |> String.toLower


diffItems : Config msg idType itemType -> List itemType -> List itemType -> List itemType
diffItems config a b =
    let
        isEqual itemA itemB =
            config.toId itemA == config.toId itemB.id

        notInB b item =
            (List.any (isEqual item) b)
                |> not
    in
        List.filter (notInB b) a


updateInput : Config msg idType itemType -> String -> Items itemType -> State -> State
updateInput config string items state =
    let
        status =
            if (String.length string == 0) then
                Idle
            else
                Editing
    in
        { state | status = status }


updateBox : Config msg idType itemType -> Items itemType -> Int -> State -> State
updateBox config items keyCode state =
    if List.length items.selectedItems == config.maxItems then
        state
    else
        case keyCode of
            -- up
            38 ->
                { state | boxPosition = (max -1 (state.boxPosition - 1)) }

            -- down
            40 ->
                { state
                    | boxPosition =
                        (min ((List.length items.boxItems) - 1)
                            (state.boxPosition + 1)
                        )
                }

            -- enter
            13 ->
                { state | status = Cleared, boxPosition = -1 }

            _ ->
                state



-- updateKey : Int -> Model idType -> ( Model idType, Cmd (Msg idType) )
-- updateKey keyCode model =
--     case model.status of
--         Editing ->
--             updateBox keyCode model
--
--         Initial ->
--             updateBoxInitial keyCode model
--
--         Idle ->
--             updateBoxInitial keyCode model
--
--         Cleared ->
--             updateBoxInitial keyCode model
--
--         Blurred ->
--             model ! []
--
--
-- update : Msg idType -> Model idType -> ( Model idType, Cmd (Msg idType) )
-- update msg model =
--     case msg of
--         Input string ->
--             updateInput string model
--
--         KeyDown code ->
--             updateKey code model
--
--         KeyUp code ->
--             if model.status == Cleared && code == 13 then
--                 { model | status = Idle } ! []
--             else
--                 model ! []
--
--         MouseClick item ->
--             updateSelectedItem item model
--
--         Blur ->
--             { model
--                 | status = Blurred
--                 , boxPosition = 0
--                 , boxItems = defaultItems model.boxLength model.availableItems model.selectedItems
--             }
--                 ! []
--
--         Focus ->
--             { model
--                 | status = Initial
--                 , boxPosition = 0
--                 , boxItems = defaultItems model.boxLength model.availableItems model.selectedItems
--             }
--                 ! []
-- VIEW


itemView : HtmlOptions -> Bool -> Item idType -> Html msg
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


view : Config msg idType itemType -> List itemType -> List itemType -> List itemType -> State -> Html msg
view config selectedItems availableItems fallbackItems state =
    if List.length availableItems == 0 then
        div [ class config.htmlOptions.classes.container ]
            [ div [ class config.htmlOptions.classes.noOptions ] [ text config.htmlOptions.noOptions ] ]
    else
        let
            h =
                config.htmlOptions

            boxItems =
                buildBoxItems config state selectedItems availableItems

            editInput =
                case state.status of
                    Initial ->
                        if (List.length selectedItems) < config.maxItems then
                            input [ onBlur config, onInput config ] []
                        else
                            input [ onBlur config, onInput config, maxlength 0 ] []

                    Idle ->
                        if (List.length selectedItems) < config.maxItems then
                            input [ onBlur config, onInput config ] []
                        else
                            input [ onBlur config, onInput config, maxlength 0 ] []

                    Editing ->
                        let
                            maxlength' =
                                if List.length boxItems == 0 then
                                    0
                                else
                                    524288
                        in
                            input [ maxlength maxlength', onBlur Blur, onInput Input, class h.classes.inputEditing ] []

                    Cleared ->
                        input [ onKeyUp config, value "", onBlur Blur, onInput Input ] []

                    Blurred ->
                        input [ maxlength 0, onFocus Focus, value "" ] []
        in
            div [ class h.classes.container ]
                [ label
                    [ classList
                        [ ( h.classes.singleItemContainer, config.maxItems == 1 )
                        , ( h.classes.multiItemContainer, config.maxItems > 1 )
                        ]
                    ]
                    [ span [ class h.classes.selectBox, onKeyDown config ]
                        [ span [] [ itemsView h fallbackItems selectedItems state ]
                        , editInput
                        ]
                    , boxView h state
                    ]
                ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger Html.Events.keyCode)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger Html.Events.keyCode)
