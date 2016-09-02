module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Selectize exposing (selectizeItem)
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
    { selectize : Selectize.Model CurrencyRec
    }


init : ( Model, Cmd Msg )
init =
    let
        ( selectizeModel, selectizeCmd ) =
            Selectize.init [] currencies
    in
        { selectize = selectizeModel } ! [ Cmd.map SelectizeMsg selectizeCmd ]



-- UPDATE


type Msg
    = Added String
    | Removed String
    | SelectizeMsg Selectize.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectizeMsg selectizeMsg ->
            let
                ( selectizeModel, selectizeCmd ) =
                    Selectize.update selectizeMsg model.selectize
            in
                { model | selectize = selectizeModel } ! [ Cmd.map SelectizeMsg selectizeCmd ]

        _ ->
            let
                _ =
                    List.map (Debug.log "DEBUG6") currencies
            in
                model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.App.map SelectizeMsg (Selectize.view model.selectize)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


currencies : List (Selectize.Item CurrencyRec)
currencies =
    [ { itemType = Currency "AFN"
      , code = "AFN"
      , display = "Afghani"
      , searchWords = []
      }
    , { itemType = Currency "EUR"
      , code = "EUR"
      , display = "Euro"
      , searchWords = []
      }
      -- , { itemType = Currency "ALL"
      --   , code = "ALL"
      --   , display = "Lek"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "DZD"
      --   , code = "DZD"
      --   , display = "Algerian Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "USD"
      --   , code = "USD"
      --   , display = "US Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "AOA"
      --   , code = "AOA"
      --   , display = "Kwanza"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ARS"
      --   , code = "ARS"
      --   , display = "Argentine Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "AMD"
      --   , code = "AMD"
      --   , display = "Armenian Dram"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "AWG"
      --   , code = "AWG"
      --   , display = "Aruban Florin"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "AUD"
      --   , code = "AUD"
      --   , display = "Australian Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "AZN"
      --   , code = "AZN"
      --   , display = "Azerbaijanian Manat"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BSD"
      --   , code = "BSD"
      --   , display = "Bahamian Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BHD"
      --   , code = "BHD"
      --   , display = "Bahraini Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BDT"
      --   , code = "BDT"
      --   , display = "Taka"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BBD"
      --   , code = "BBD"
      --   , display = "Barbados Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BYN"
      --   , code = "BYN"
      --   , display = "Belarusian Ruble"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BYR"
      --   , code = "BYR"
      --   , display = "Belarusian Ruble"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BZD"
      --   , code = "BZD"
      --   , display = "Belize Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BMD"
      --   , code = "BMD"
      --   , display = "Bermudian Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "INR"
      --   , code = "INR"
      --   , display = "Indian Rupee"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BTN"
      --   , code = "BTN"
      --   , display = "Ngultrum"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BOB"
      --   , code = "BOB"
      --   , display = "Boliviano"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BOV"
      --   , code = "BOV"
      --   , display = "Mvdol"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BAM"
      --   , code = "BAM"
      --   , display = "Convertible Mark"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BWP"
      --   , code = "BWP"
      --   , display = "Pula"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "NOK"
      --   , code = "NOK"
      --   , display = "Norwegian Krone"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BRL"
      --   , code = "BRL"
      --   , display = "Brazilian Real"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BND"
      --   , code = "BND"
      --   , display = "Brunei Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BGN"
      --   , code = "BGN"
      --   , display = "Bulgarian Lev"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "BIF"
      --   , code = "BIF"
      --   , display = "Burundi Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CVE"
      --   , code = "CVE"
      --   , display = "Cabo Verde Escudo"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KHR"
      --   , code = "KHR"
      --   , display = "Riel"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CAD"
      --   , code = "CAD"
      --   , display = "Canadian Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KYD"
      --   , code = "KYD"
      --   , display = "Cayman Islands Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CLP"
      --   , code = "CLP"
      --   , display = "Chilean Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CLF"
      --   , code = "CLF"
      --   , display = "Unidad de Fomento"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CNY"
      --   , code = "CNY"
      --   , display = "Yuan Renminbi"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "COP"
      --   , code = "COP"
      --   , display = "Colombian Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "COU"
      --   , code = "COU"
      --   , display = "Unidad de Valor Real"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KMF"
      --   , code = "KMF"
      --   , display = "Comoro Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CDF"
      --   , code = "CDF"
      --   , display = "Congolese Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "NZD"
      --   , code = "NZD"
      --   , display = "New Zealand Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CRC"
      --   , code = "CRC"
      --   , display = "Costa Rican Colon"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "HRK"
      --   , code = "HRK"
      --   , display = "Kuna"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CUP"
      --   , code = "CUP"
      --   , display = "Cuban Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CUC"
      --   , code = "CUC"
      --   , display = "Peso Convertible"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ANG"
      --   , code = "ANG"
      --   , display = "Netherlands Antillean Guilder"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CZK"
      --   , code = "CZK"
      --   , display = "Czech Koruna"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "DKK"
      --   , code = "DKK"
      --   , display = "Danish Krone"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "DJF"
      --   , code = "DJF"
      --   , display = "Djibouti Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "DOP"
      --   , code = "DOP"
      --   , display = "Dominican Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "EGP"
      --   , code = "EGP"
      --   , display = "Egyptian Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SVC"
      --   , code = "SVC"
      --   , display = "El Salvador Colon"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ERN"
      --   , code = "ERN"
      --   , display = "Nakfa"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ETB"
      --   , code = "ETB"
      --   , display = "Ethiopian Birr"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "FKP"
      --   , code = "FKP"
      --   , display = "Falkland Islands Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "FJD"
      --   , code = "FJD"
      --   , display = "Fiji Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GMD"
      --   , code = "GMD"
      --   , display = "Dalasi"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GEL"
      --   , code = "GEL"
      --   , display = "Lari"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GHS"
      --   , code = "GHS"
      --   , display = "Ghana Cedi"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GIP"
      --   , code = "GIP"
      --   , display = "Gibraltar Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GTQ"
      --   , code = "GTQ"
      --   , display = "Quetzal"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GBP"
      --   , code = "GBP"
      --   , display = "Pound Sterling"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GNF"
      --   , code = "GNF"
      --   , display = "Guinea Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "GYD"
      --   , code = "GYD"
      --   , display = "Guyana Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "HTG"
      --   , code = "HTG"
      --   , display = "Gourde"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "HNL"
      --   , code = "HNL"
      --   , display = "Lempira"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "HKD"
      --   , code = "HKD"
      --   , display = "Hong Kong Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "HUF"
      --   , code = "HUF"
      --   , display = "Forint"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ISK"
      --   , code = "ISK"
      --   , display = "Iceland Krona"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "IDR"
      --   , code = "IDR"
      --   , display = "Rupiah"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "IRR"
      --   , code = "IRR"
      --   , display = "Iranian Rial"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "IQD"
      --   , code = "IQD"
      --   , display = "Iraqi Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ILS"
      --   , code = "ILS"
      --   , display = "New Israeli Sheqel"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "JMD"
      --   , code = "JMD"
      --   , display = "Jamaican Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "JPY"
      --   , code = "JPY"
      --   , display = "Yen"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "JOD"
      --   , code = "JOD"
      --   , display = "Jordanian Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KZT"
      --   , code = "KZT"
      --   , display = "Tenge"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KES"
      --   , code = "KES"
      --   , display = "Kenyan Shilling"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KPW"
      --   , code = "KPW"
      --   , display = "North Korean Won"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KRW"
      --   , code = "KRW"
      --   , display = "Won"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KWD"
      --   , code = "KWD"
      --   , display = "Kuwaiti Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "KGS"
      --   , code = "KGS"
      --   , display = "Som"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "LAK"
      --   , code = "LAK"
      --   , display = "Kip"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "LBP"
      --   , code = "LBP"
      --   , display = "Lebanese Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "LSL"
      --   , code = "LSL"
      --   , display = "Loti"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ZAR"
      --   , code = "ZAR"
      --   , display = "Rand"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "LRD"
      --   , code = "LRD"
      --   , display = "Liberian Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "LYD"
      --   , code = "LYD"
      --   , display = "Libyan Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CHF"
      --   , code = "CHF"
      --   , display = "Swiss Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MOP"
      --   , code = "MOP"
      --   , display = "Pataca"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MKD"
      --   , code = "MKD"
      --   , display = "Denar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MGA"
      --   , code = "MGA"
      --   , display = "Malagasy Ariary"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MWK"
      --   , code = "MWK"
      --   , display = "Malawi Kwacha"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MYR"
      --   , code = "MYR"
      --   , display = "Malaysian Ringgit"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MVR"
      --   , code = "MVR"
      --   , display = "Rufiyaa"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MRO"
      --   , code = "MRO"
      --   , display = "Ouguiya"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MUR"
      --   , code = "MUR"
      --   , display = "Mauritius Rupee"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MXN"
      --   , code = "MXN"
      --   , display = "Mexican Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MXV"
      --   , code = "MXV"
      --   , display = "Mexican Unidad de Inversion (UDI)"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MDL"
      --   , code = "MDL"
      --   , display = "Moldovan Leu"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MNT"
      --   , code = "MNT"
      --   , display = "Tugrik"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MAD"
      --   , code = "MAD"
      --   , display = "Moroccan Dirham"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MZN"
      --   , code = "MZN"
      --   , display = "Mozambique Metical"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "MMK"
      --   , code = "MMK"
      --   , display = "Kyat"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "NAD"
      --   , code = "NAD"
      --   , display = "Namibia Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "NPR"
      --   , code = "NPR"
      --   , display = "Nepalese Rupee"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "NIO"
      --   , code = "NIO"
      --   , display = "Cordoba Oro"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "NGN"
      --   , code = "NGN"
      --   , display = "Naira"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "OMR"
      --   , code = "OMR"
      --   , display = "Rial Omani"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PKR"
      --   , code = "PKR"
      --   , display = "Pakistan Rupee"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PAB"
      --   , code = "PAB"
      --   , display = "Balboa"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PGK"
      --   , code = "PGK"
      --   , display = "Kina"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PYG"
      --   , code = "PYG"
      --   , display = "Guarani"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PEN"
      --   , code = "PEN"
      --   , display = "Sol"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PHP"
      --   , code = "PHP"
      --   , display = "Philippine Peso"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "PLN"
      --   , code = "PLN"
      --   , display = "Zloty"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "QAR"
      --   , code = "QAR"
      --   , display = "Qatari Rial"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "RON"
      --   , code = "RON"
      --   , display = "Romanian Leu"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "RUB"
      --   , code = "RUB"
      --   , display = "Russian Ruble"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "RWF"
      --   , code = "RWF"
      --   , display = "Rwanda Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SHP"
      --   , code = "SHP"
      --   , display = "Saint Helena Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "WST"
      --   , code = "WST"
      --   , display = "Tala"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "STD"
      --   , code = "STD"
      --   , display = "Dobra"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SAR"
      --   , code = "SAR"
      --   , display = "Saudi Riyal"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "RSD"
      --   , code = "RSD"
      --   , display = "Serbian Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SCR"
      --   , code = "SCR"
      --   , display = "Seychelles Rupee"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SLL"
      --   , code = "SLL"
      --   , display = "Leone"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SGD"
      --   , code = "SGD"
      --   , display = "Singapore Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SBD"
      --   , code = "SBD"
      --   , display = "Solomon Islands Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SOS"
      --   , code = "SOS"
      --   , display = "Somali Shilling"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SSP"
      --   , code = "SSP"
      --   , display = "South Sudanese Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "LKR"
      --   , code = "LKR"
      --   , display = "Sri Lanka Rupee"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SDG"
      --   , code = "SDG"
      --   , display = "Sudanese Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SRD"
      --   , code = "SRD"
      --   , display = "Surinam Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SZL"
      --   , code = "SZL"
      --   , display = "Lilangeni"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SEK"
      --   , code = "SEK"
      --   , display = "Swedish Krona"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CHE"
      --   , code = "CHE"
      --   , display = "WIR Euro"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "CHW"
      --   , code = "CHW"
      --   , display = "WIR Franc"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "SYP"
      --   , code = "SYP"
      --   , display = "Syrian Pound"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TWD"
      --   , code = "TWD"
      --   , display = "New Taiwan Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TJS"
      --   , code = "TJS"
      --   , display = "Somoni"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TZS"
      --   , code = "TZS"
      --   , display = "Tanzanian Shilling"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "THB"
      --   , code = "THB"
      --   , display = "Baht"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TOP"
      --   , code = "TOP"
      --   , display = "Pa’anga"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TTD"
      --   , code = "TTD"
      --   , display = "Trinidad and Tobago Dollar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TND"
      --   , code = "TND"
      --   , display = "Tunisian Dinar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TRY"
      --   , code = "TRY"
      --   , display = "Turkish Lira"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "TMT"
      --   , code = "TMT"
      --   , display = "Turkmenistan New Manat"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "UGX"
      --   , code = "UGX"
      --   , display = "Uganda Shilling"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "UAH"
      --   , code = "UAH"
      --   , display = "Hryvnia"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "AED"
      --   , code = "AED"
      --   , display = "UAE Dirham"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "USN"
      --   , code = "USN"
      --   , display = "US Dollar (Next day)"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "UYU"
      --   , code = "UYU"
      --   , display = "Peso Uruguayo"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "UYI"
      --   , code = "UYI"
      --   , display = "Uruguay Peso en Unidades Indexadas (URUIURUI)"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "UZS"
      --   , code = "UZS"
      --   , display = "Uzbekistan Sum"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "VUV"
      --   , code = "VUV"
      --   , display = "Vatu"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "VEF"
      --   , code = "VEF"
      --   , display = "Bolívar"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "VND"
      --   , code = "VND"
      --   , display = "Dong"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "YER"
      --   , code = "YER"
      --   , display = "Yemeni Rial"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ZMW"
      --   , code = "ZMW"
      --   , display = "Zambian Kwacha"
      --   , searchWords = []
      --   }
      -- , { itemType = Currency "ZWL"
      --   , code = "ZWL"
      --   , display = "Zimbabwe Dollar"
      --   , searchWords = []
      --   }
    ]
