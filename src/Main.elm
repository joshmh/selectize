module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Selectize
import Html.App


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type CurrencyRec
    = Currency String


type alias Model =
    { selectizeState : Selectize.State
    }


type alias Item =
    { code : String
    , display : String
    }


selectedCodes : List String
selectedCodes =
    [ "USD", "ILS", "CAD" ]


selectedItems : List Item
selectedItems =
    List.filter (\item -> List.member item.code selectedCodes) availableItems


mapCurrency : { a | code : String, display : String } -> Item
mapCurrency rawCurrency =
    { code = rawCurrency.code
    , display = rawCurrency.display
    }


availableItems : List Item
availableItems =
    List.map mapCurrency rawCurrencies


init : ( Model, Cmd Msg )
init =
    { selectizeState = Selectize.initialSelectize } ! []



-- UPDATE


type Msg
    = SelectizeMsg Selectize.State
    | Focus Selectize.State
    | Blur Selectize.State
    | Add String Selectize.State
    | Remove Selectize.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectizeMsg state ->
            { selectizeState = state } ! []

        Focus state ->
            { selectizeState = state } ! []

        Blur state ->
            { selectizeState = state } ! []

        Add code state ->
            { selectizeState = state } ! []

        Remove state ->
            { selectizeState = state } ! []



-- VIEW


match : String -> List Item
match string =
    []


config : Selectize.Config Msg String Item
config =
    { maxItems = 3
    , boxLength = 5
    , toMsg = SelectizeMsg
    , onAdd = Add
    , onRemove = Remove
    , onFocus = Focus
    , onBlur = Blur
    , toId = .code
    , toDisplay = .display
    , match = match
    , htmlOptions =
        { instructionsForBlank = "Start typing for options"
        , noMatches = "No matches"
        , atMaxLength = "Type backspace to edit"
        , typeForMore = "Type for more options"
        , noOptions = "No options"
        , classes =
            { container = "container"
            , noOptions = "noOptions"
            , singleItemContainer = "singleItemContainer"
            , multiItemContainer = "multiItemContainer"
            , selectedItems = "selectedItems"
            , fallbackItems = "fallbackItems"
            , fallbackItem = "fallbackItem"
            , selectedItem = "selectedItem"
            , boxContainer = "boxContainer"
            , boxItems = "boxItems"
            , boxItem = "boxItem"
            , boxItemActive = "activeBoxItem"
            , selectBox = "selectBox"
            , info = "info"
            , infoNoMatches = "infoNoMatches"
            , inputEditing = "inputEditing"
            }
        }
    }


fallbackCodes : List String
fallbackCodes =
    []


fallbackItems : List Item
fallbackItems =
    List.filter (\item -> List.member item.code fallbackCodes) availableItems



-- [ "INR", "BTN", "BOB" ]


view : Model -> Html Msg
view model =
    div []
        [ Selectize.view config selectedItems availableItems fallbackItems model.selectizeState
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


rawCurrencies : List { code : String, display : String, searchWords : List a }
rawCurrencies =
    [ { code = "AFN"
      , display = "Afghani"
      , searchWords = []
      }
    , { code = "EUR"
      , display = "Euro"
      , searchWords = []
      }
    , { code = "ALL"
      , display = "Lek"
      , searchWords = []
      }
    , { code = "DZD"
      , display = "Algerian Dinar"
      , searchWords = []
      }
    , { code = "USD"
      , display = "US Dollar"
      , searchWords = []
      }
    , { code = "AOA"
      , display = "Kwanza"
      , searchWords = []
      }
    , { code = "ARS"
      , display = "Argentine Peso"
      , searchWords = []
      }
    , { code = "AMD"
      , display = "Armenian Dram"
      , searchWords = []
      }
    , { code = "AWG"
      , display = "Aruban Florin"
      , searchWords = []
      }
    , { code = "AUD"
      , display = "Australian Dollar"
      , searchWords = []
      }
    , { code = "AZN"
      , display = "Azerbaijanian Manat"
      , searchWords = []
      }
    , { code = "BSD"
      , display = "Bahamian Dollar"
      , searchWords = []
      }
    , { code = "BHD"
      , display = "Bahraini Dinar"
      , searchWords = []
      }
    , { code = "BDT"
      , display = "Taka"
      , searchWords = []
      }
    , { code = "BBD"
      , display = "Barbados Dollar"
      , searchWords = []
      }
    , { code = "BYN"
      , display = "Belarusian Ruble"
      , searchWords = []
      }
    , { code = "BYR"
      , display = "Belarusian Ruble"
      , searchWords = []
      }
    , { code = "BZD"
      , display = "Belize Dollar"
      , searchWords = []
      }
    , { code = "BMD"
      , display = "Bermudian Dollar"
      , searchWords = []
      }
    , { code = "INR"
      , display = "Indian Rupee"
      , searchWords = []
      }
    , { code = "BTN"
      , display = "Ngultrum"
      , searchWords = []
      }
    , { code = "BOB"
      , display = "Boliviano"
      , searchWords = []
      }
    , { code = "BOV"
      , display = "Mvdol"
      , searchWords = []
      }
    , { code = "BAM"
      , display = "Convertible Mark"
      , searchWords = []
      }
    , { code = "BWP"
      , display = "Pula"
      , searchWords = []
      }
    , { code = "NOK"
      , display = "Norwegian Krone"
      , searchWords = []
      }
    , { code = "BRL"
      , display = "Brazilian Real"
      , searchWords = []
      }
    , { code = "BND"
      , display = "Brunei Dollar"
      , searchWords = []
      }
    , { code = "BGN"
      , display = "Bulgarian Lev"
      , searchWords = []
      }
    , { code = "BIF"
      , display = "Burundi Franc"
      , searchWords = []
      }
    , { code = "CVE"
      , display = "Cabo Verde Escudo"
      , searchWords = []
      }
    , { code = "KHR"
      , display = "Riel"
      , searchWords = []
      }
    , { code = "CAD"
      , display = "Canadian Dollar"
      , searchWords = []
      }
    , { code = "KYD"
      , display = "Cayman Islands Dollar"
      , searchWords = []
      }
    , { code = "CLP"
      , display = "Chilean Peso"
      , searchWords = []
      }
    , { code = "CLF"
      , display = "Unidad de Fomento"
      , searchWords = []
      }
    , { code = "CNY"
      , display = "Yuan Renminbi"
      , searchWords = []
      }
    , { code = "COP"
      , display = "Colombian Peso"
      , searchWords = []
      }
    , { code = "COU"
      , display = "Unidad de Valor Real"
      , searchWords = []
      }
    , { code = "KMF"
      , display = "Comoro Franc"
      , searchWords = []
      }
    , { code = "CDF"
      , display = "Congolese Franc"
      , searchWords = []
      }
    , { code = "NZD"
      , display = "New Zealand Dollar"
      , searchWords = []
      }
    , { code = "CRC"
      , display = "Costa Rican Colon"
      , searchWords = []
      }
    , { code = "HRK"
      , display = "Kuna"
      , searchWords = []
      }
    , { code = "CUP"
      , display = "Cuban Peso"
      , searchWords = []
      }
    , { code = "CUC"
      , display = "Peso Convertible"
      , searchWords = []
      }
    , { code = "ANG"
      , display = "Netherlands Antillean Guilder"
      , searchWords = []
      }
    , { code = "CZK"
      , display = "Czech Koruna"
      , searchWords = []
      }
    , { code = "DKK"
      , display = "Danish Krone"
      , searchWords = []
      }
    , { code = "DJF"
      , display = "Djibouti Franc"
      , searchWords = []
      }
    , { code = "DOP"
      , display = "Dominican Peso"
      , searchWords = []
      }
    , { code = "EGP"
      , display = "Egyptian Pound"
      , searchWords = []
      }
    , { code = "SVC"
      , display = "El Salvador Colon"
      , searchWords = []
      }
    , { code = "ERN"
      , display = "Nakfa"
      , searchWords = []
      }
    , { code = "ETB"
      , display = "Ethiopian Birr"
      , searchWords = []
      }
    , { code = "FKP"
      , display = "Falkland Islands Pound"
      , searchWords = []
      }
    , { code = "FJD"
      , display = "Fiji Dollar"
      , searchWords = []
      }
    , { code = "GMD"
      , display = "Dalasi"
      , searchWords = []
      }
    , { code = "GEL"
      , display = "Lari"
      , searchWords = []
      }
    , { code = "GHS"
      , display = "Ghana Cedi"
      , searchWords = []
      }
    , { code = "GIP"
      , display = "Gibraltar Pound"
      , searchWords = []
      }
    , { code = "GTQ"
      , display = "Quetzal"
      , searchWords = []
      }
    , { code = "GBP"
      , display = "Pound Sterling"
      , searchWords = []
      }
    , { code = "GNF"
      , display = "Guinea Franc"
      , searchWords = []
      }
    , { code = "GYD"
      , display = "Guyana Dollar"
      , searchWords = []
      }
    , { code = "HTG"
      , display = "Gourde"
      , searchWords = []
      }
    , { code = "HNL"
      , display = "Lempira"
      , searchWords = []
      }
    , { code = "HKD"
      , display = "Hong Kong Dollar"
      , searchWords = []
      }
    , { code = "HUF"
      , display = "Forint"
      , searchWords = []
      }
    , { code = "ISK"
      , display = "Iceland Krona"
      , searchWords = []
      }
    , { code = "IDR"
      , display = "Rupiah"
      , searchWords = []
      }
    , { code = "IRR"
      , display = "Iranian Rial"
      , searchWords = []
      }
    , { code = "IQD"
      , display = "Iraqi Dinar"
      , searchWords = []
      }
    , { code = "ILS"
      , display = "New Israeli Sheqel"
      , searchWords = []
      }
    , { code = "JMD"
      , display = "Jamaican Dollar"
      , searchWords = []
      }
    , { code = "JPY"
      , display = "Yen"
      , searchWords = []
      }
    , { code = "JOD"
      , display = "Jordanian Dinar"
      , searchWords = []
      }
    , { code = "KZT"
      , display = "Tenge"
      , searchWords = []
      }
    , { code = "KES"
      , display = "Kenyan Shilling"
      , searchWords = []
      }
    , { code = "KPW"
      , display = "North Korean Won"
      , searchWords = []
      }
    , { code = "KRW"
      , display = "Won"
      , searchWords = []
      }
    , { code = "KWD"
      , display = "Kuwaiti Dinar"
      , searchWords = []
      }
    , { code = "KGS"
      , display = "Som"
      , searchWords = []
      }
    , { code = "LAK"
      , display = "Kip"
      , searchWords = []
      }
    , { code = "LBP"
      , display = "Lebanese Pound"
      , searchWords = []
      }
    , { code = "LSL"
      , display = "Loti"
      , searchWords = []
      }
    , { code = "ZAR"
      , display = "Rand"
      , searchWords = []
      }
    , { code = "LRD"
      , display = "Liberian Dollar"
      , searchWords = []
      }
    , { code = "LYD"
      , display = "Libyan Dinar"
      , searchWords = []
      }
    , { code = "CHF"
      , display = "Swiss Franc"
      , searchWords = []
      }
    , { code = "MOP"
      , display = "Pataca"
      , searchWords = []
      }
    , { code = "MKD"
      , display = "Denar"
      , searchWords = []
      }
    , { code = "MGA"
      , display = "Malagasy Ariary"
      , searchWords = []
      }
    , { code = "MWK"
      , display = "Malawi Kwacha"
      , searchWords = []
      }
    , { code = "MYR"
      , display = "Malaysian Ringgit"
      , searchWords = []
      }
    , { code = "MVR"
      , display = "Rufiyaa"
      , searchWords = []
      }
    , { code = "MRO"
      , display = "Ouguiya"
      , searchWords = []
      }
    , { code = "MUR"
      , display = "Mauritius Rupee"
      , searchWords = []
      }
    , { code = "MXN"
      , display = "Mexican Peso"
      , searchWords = []
      }
    , { code = "MXV"
      , display = "Mexican Unidad de Inversion (UDI)"
      , searchWords = []
      }
    , { code = "MDL"
      , display = "Moldovan Leu"
      , searchWords = []
      }
    , { code = "MNT"
      , display = "Tugrik"
      , searchWords = []
      }
    , { code = "MAD"
      , display = "Moroccan Dirham"
      , searchWords = []
      }
    , { code = "MZN"
      , display = "Mozambique Metical"
      , searchWords = []
      }
    , { code = "MMK"
      , display = "Kyat"
      , searchWords = []
      }
    , { code = "NAD"
      , display = "Namibia Dollar"
      , searchWords = []
      }
    , { code = "NPR"
      , display = "Nepalese Rupee"
      , searchWords = []
      }
    , { code = "NIO"
      , display = "Cordoba Oro"
      , searchWords = []
      }
    , { code = "NGN"
      , display = "Naira"
      , searchWords = []
      }
    , { code = "OMR"
      , display = "Rial Omani"
      , searchWords = []
      }
    , { code = "PKR"
      , display = "Pakistan Rupee"
      , searchWords = []
      }
    , { code = "PAB"
      , display = "Balboa"
      , searchWords = []
      }
    , { code = "PGK"
      , display = "Kina"
      , searchWords = []
      }
    , { code = "PYG"
      , display = "Guarani"
      , searchWords = []
      }
    , { code = "PEN"
      , display = "Sol"
      , searchWords = []
      }
    , { code = "PHP"
      , display = "Philippine Peso"
      , searchWords = []
      }
    , { code = "PLN"
      , display = "Zloty"
      , searchWords = []
      }
    , { code = "QAR"
      , display = "Qatari Rial"
      , searchWords = []
      }
    , { code = "RON"
      , display = "Romanian Leu"
      , searchWords = []
      }
    , { code = "RUB"
      , display = "Russian Ruble"
      , searchWords = []
      }
    , { code = "RWF"
      , display = "Rwanda Franc"
      , searchWords = []
      }
    , { code = "SHP"
      , display = "Saint Helena Pound"
      , searchWords = []
      }
    , { code = "WST"
      , display = "Tala"
      , searchWords = []
      }
    , { code = "STD"
      , display = "Dobra"
      , searchWords = []
      }
    , { code = "SAR"
      , display = "Saudi Riyal"
      , searchWords = []
      }
    , { code = "RSD"
      , display = "Serbian Dinar"
      , searchWords = []
      }
    , { code = "SCR"
      , display = "Seychelles Rupee"
      , searchWords = []
      }
    , { code = "SLL"
      , display = "Leone"
      , searchWords = []
      }
    , { code = "SGD"
      , display = "Singapore Dollar"
      , searchWords = []
      }
    , { code = "SBD"
      , display = "Solomon Islands Dollar"
      , searchWords = []
      }
    , { code = "SOS"
      , display = "Somali Shilling"
      , searchWords = []
      }
    , { code = "SSP"
      , display = "South Sudanese Pound"
      , searchWords = []
      }
    , { code = "LKR"
      , display = "Sri Lanka Rupee"
      , searchWords = []
      }
    , { code = "SDG"
      , display = "Sudanese Pound"
      , searchWords = []
      }
    , { code = "SRD"
      , display = "Surinam Dollar"
      , searchWords = []
      }
    , { code = "SZL"
      , display = "Lilangeni"
      , searchWords = []
      }
    , { code = "SEK"
      , display = "Swedish Krona"
      , searchWords = []
      }
    , { code = "CHE"
      , display = "WIR Euro"
      , searchWords = []
      }
    , { code = "CHW"
      , display = "WIR Franc"
      , searchWords = []
      }
    , { code = "SYP"
      , display = "Syrian Pound"
      , searchWords = []
      }
    , { code = "TWD"
      , display = "New Taiwan Dollar"
      , searchWords = []
      }
    , { code = "TJS"
      , display = "Somoni"
      , searchWords = []
      }
    , { code = "TZS"
      , display = "Tanzanian Shilling"
      , searchWords = []
      }
    , { code = "THB"
      , display = "Baht"
      , searchWords = []
      }
    , { code = "TOP"
      , display = "Pa’anga"
      , searchWords = []
      }
    , { code = "TTD"
      , display = "Trinidad and Tobago Dollar"
      , searchWords = []
      }
    , { code = "TND"
      , display = "Tunisian Dinar"
      , searchWords = []
      }
    , { code = "TRY"
      , display = "Turkish Lira"
      , searchWords = []
      }
    , { code = "TMT"
      , display = "Turkmenistan New Manat"
      , searchWords = []
      }
    , { code = "UGX"
      , display = "Uganda Shilling"
      , searchWords = []
      }
    , { code = "UAH"
      , display = "Hryvnia"
      , searchWords = []
      }
    , { code = "AED"
      , display = "UAE Dirham"
      , searchWords = []
      }
    , { code = "USN"
      , display = "US Dollar (Next day)"
      , searchWords = []
      }
    , { code = "UYU"
      , display = "Peso Uruguayo"
      , searchWords = []
      }
    , { code = "UYI"
      , display = "Uruguay Peso en Unidades Indexadas (URUIURUI)"
      , searchWords = []
      }
    , { code = "UZS"
      , display = "Uzbekistan Sum"
      , searchWords = []
      }
    , { code = "VUV"
      , display = "Vatu"
      , searchWords = []
      }
    , { code = "VEF"
      , display = "Bolívar"
      , searchWords = []
      }
    , { code = "VND"
      , display = "Dong"
      , searchWords = []
      }
    , { code = "YER"
      , display = "Yemeni Rial"
      , searchWords = []
      }
    , { code = "ZMW"
      , display = "Zambian Kwacha"
      , searchWords = []
      }
    , { code = "ZWL"
      , display = "Zimbabwe Dollar"
      , searchWords = []
      }
    ]
