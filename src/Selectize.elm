module Selectize
    exposing
        ( initialSelectize
        , view
        , State
        , Config
        )

import Html exposing (..)
import Html.Attributes exposing (value, defaultValue, maxlength, class, classList, id)
import Html.Events as E exposing (on)
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
    , match : String -> List itemType
    , htmlOptions : HtmlOptions
    }


type alias Items itemType =
    { selectedItems : List itemType
    , availableItems : List itemType
    , boxItems : List itemType
    }


initialSelectize : State
initialSelectize =
    { boxPosition = -1, state = Blurred }



-- UPDATE


clean : String -> String
clean s =
    String.trim s
        |> String.toLower


updateInput : String -> State
updateInput string state =
    let
        status =
            if (String.length string == 0) then
                Idle
            else
                Editing
    in
        { state | status = status }


updateKeyUp : Config msg idType itemType -> Items itemType -> State -> Int -> msg
updateKeyUp config items state keyCode =
    if keyCode == 13 then
        config.toMsg { state | status = Initial }
    else
        config.toMsg state


updateKeyDown : Config msg idType itemType -> Items itemType -> State -> Int -> msg
updateKeyDown config items state keyCode =
    if List.length items.selectedItems == config.maxItems then
        state
    else
        case keyCode of
            -- up
            38 ->
                config.toMsg { state | boxPosition = (max -1 (state.boxPosition - 1)) }

            -- down
            40 ->
                config.toMsg
                    { state
                        | boxPosition =
                            (min ((List.length items.boxItems) - 1)
                                (state.boxPosition + 1)
                            )
                    }

            -- enter
            13 ->
                let
                    maybeItem =
                        (List.head << (List.drop state.boxPosition)) items.boxItems
                in
                    case maybeItem of
                        Nothing ->
                            config.toMsg state

                        Just item ->
                            config.onAdd (config.toId item) { state | status = Cleared, boxPosition = -1 }

            _ ->
                config.toMsg state



-- VIEW


itemView : Config msg idType itemType -> Bool -> itemType -> Html msg
itemView config isFallback item =
    let
        c =
            config.htmlOptions.classes
    in
        span
            [ classList
                [ ( c.selectedItem, True )
                , ( c.fallbackItem, isFallback )
                ]
            ]
            [ text (config.toDisplay item) ]


fallbackItemsView : Config msg idType itemType -> Items itemType -> List itemType -> State -> Html msg
fallbackItemsView config items fallbackItems state =
    let
        c =
            config.htmlOptions.classes

        classes =
            classList
                [ ( c.selectedItems, True )
                , ( c.fallbackItems, List.length items.selectedItems == 0 )
                ]

        isFallback =
            List.length items.selectedItems == 0

        items =
            if isFallback then
                fallbackItems
            else
                items.selectedItems
    in
        span [ classes ] (List.map (itemView config isFallback) items)


itemsView : Config msg idType itemType -> Items itemType -> List itemType -> State -> Html msg
itemsView config items fallbackItems state =
    case state.status of
        Editing ->
            fallbackItemsView config items [] state

        Initial ->
            fallbackItemsView config items fallbackItems state

        Idle ->
            fallbackItemsView config items fallbackItems state

        Cleared ->
            fallbackItemsView config items fallbackItems state

        Blurred ->
            fallbackItemsView config items fallbackItems state


editingBoxView : Config msg idType itemType -> Items itemType -> State -> Html msg
editingBoxView config items state =
    let
        h =
            config.htmlOptions

        c =
            h.classes

        boxItemHtml pos item =
            div
                [ classList
                    [ ( c.boxItem, True )
                    , ( c.boxItemActive, state.boxPosition == pos )
                    ]
                , onMouseDown config state (config.toId item)
                ]
                [ text item.optionDisplay
                ]
    in
        div [ class c.boxItems ] (List.indexedMap boxItemHtml items.boxItems)


idleBoxView : Config msg idType itemType -> Items itemType -> State -> Html msg
idleBoxView config items state =
    let
        h =
            config.htmlOptions

        remainingItems =
            List.length items.availableItems - List.length items.selectedItems

        typeForMore =
            if remainingItems > config.boxLength then
                div [ class h.classes.info ] [ text h.typeForMore ]
            else
                span [] []
    in
        if List.length items.selectedItems == config.maxItems then
            span [] []
        else
            div [ class h.classes.boxContainer ]
                [ editingBoxView config items state
                , typeForMore
                ]


noMatches : Config msg idType itemType -> List itemType -> State -> Html msg
noMatches config boxItems state =
    let
        h =
            config.htmlOptions
    in
        if List.length boxItems == 0 then
            div
                [ classList
                    [ ( h.classes.info, True )
                    , ( h.classes.infoNoMatches, True )
                    ]
                ]
                [ text h.noMatches ]
        else
            span [] []


boxView : Config msg idType itemType -> Items itemType -> State -> Html msg
boxView config items state =
    let
        h =
            config.htmlOptions
    in
        case state.status of
            Editing ->
                div [ class h.classes.boxContainer ]
                    [ editingBoxView config items state
                    , noMatches config items.boxItems state
                    ]

            Initial ->
                idleBoxView config items state

            Idle ->
                idleBoxView config items state

            Cleared ->
                idleBoxView config items state

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
                config.match state.string

            onInputAtt =
                onInput config state

            onBlurAtt =
                onBlur config state

            onFocusAtt =
                onFocus config state

            editInput =
                case state.status of
                    Initial ->
                        if (List.length selectedItems) < config.maxItems then
                            input [ onBlurAtt, onInputAtt ] []
                        else
                            input [ onBlurAtt, onInputAtt, maxlength 0 ] []

                    Idle ->
                        if (List.length selectedItems) < config.maxItems then
                            input [ onBlurAtt, onInputAtt ] []
                        else
                            input [ onBlurAtt, onInputAtt, maxlength 0 ] []

                    Editing ->
                        let
                            maxlength' =
                                if List.length boxItems == 0 then
                                    0
                                else
                                    524288
                        in
                            input [ maxlength maxlength', onBlurAtt, onInputAtt, class h.classes.inputEditing ] []

                    Cleared ->
                        input [ onKeyUp config, value "", onBlurAtt, onInputAtt ] []

                    Blurred ->
                        input [ maxlength 0, onFocusAtt, value "" ] []
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


onInput : Config msg idType itemType -> State -> Attribute msg
onInput config state =
    E.onInput config.toMsg <| (\s -> { state | string = s })


onMouseDown : Config msg idType itemType -> State -> idType -> Attribute msg
onMouseDown config state id =
    E.onMouseDown (config.onAdd id)


onBlur : Config msg idType itemType -> State -> Attribute msg
onBlur config state =
    E.onBlur config.onBlur <| (\s -> { state | status = Blurred })


onFocus : Config msg idType itemType -> State -> Attribute msg
onFocus config state =
    E.onBlur config.onFocus <| (\s -> { state | status = Initial, boxPosition = -1 })


onKeyDown : Config msg idType itemType -> Items items -> State -> Attribute msg
onKeyDown config items state =
    rawOnKeyDown (updateKeyDown config items state)


onKeyUp : Config msg idType itemType -> Items items -> State -> Attribute msg
onKeyUp config items state =
    rawOnKeyUp (updateKeyUp config items state)


rawOnKeyDown : (Int -> msg) -> Attribute msg
rawOnKeyDown tagger =
    on "keydown" (Json.Decode.map tagger E.keyCode)


rawOnKeyUp : (Int -> msg) -> Attribute msg
rawOnKeyUp tagger =
    on "keyup" (Json.Decode.map tagger E.keyCode)
