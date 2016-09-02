module Selectize exposing (init, update, view, selectizeItem, Model, Msg, Item)

import Html exposing (..)
import Html.Attributes exposing (value, defaultValue)
import Html.Events exposing (onInput)
import Html.Keyed
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
    , generation = 0
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


updateInput : String -> Model x -> ( Model x, Cmd Msg )
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
                            , generation = model.generation + 1
                            , selectedItems = model.selectedItems ++ [ item ]
                        }
                            ! []

        _ ->
            model ! []


update : Maybe Int -> Maybe Msg -> Model x -> ( Model x, Cmd Msg )
update maybeKeyCode maybeMsg model =
    let
        ( model1, cmd1 ) =
            case maybeMsg of
                Nothing ->
                    model ! []

                Just msg ->
                    case msg of
                        Input string ->
                            updateInput string model
    in
        case maybeKeyCode of
            Nothing ->
                ( model1, cmd1 )

            Just keyCode ->
                let
                    ( model2, cmd2 ) =
                        updateKey keyCode model1
                in
                    model2 ! [ cmd1, cmd2 ]



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
            input [ onInput Input ] []
    in
        div []
            [ div []
                [ div [] [ itemsView (Debug.log "DEBUG1" model.selectedItems) ]
                , Html.Keyed.node "div"
                    []
                    [ ( "gen-" ++ (toString model.generation)
                      , editInput
                      )
                    ]
                ]
            , boxView model.boxPosition model.boxItems
            ]
