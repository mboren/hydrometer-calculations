module App exposing (main)

import Html exposing (..)
import Html.Attributes
import Html.Events


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Temperature =
    Float


type alias Row =
    { measuredGravity : Maybe Float
    , measuredTemperature : Maybe Temperature
    , hydrometerCalibration : Maybe Temperature
    , correctedGravity : Maybe Float
    }


type alias Model =
    { table : List Row
    }


emptyRow =
    Row Nothing Nothing Nothing Nothing


init : ( Model, Cmd Msg )
init =
    ( Model [ emptyRow ], Cmd.none )


type Msg
    = NewGravity Int String
    | NewTemperature Int String
    | NewCalibration Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewTable model.table
        ]


viewTable : List Row -> Html Msg
viewTable rows =
    div
        []
        (rows
            |> List.map stringifyRowFields
            |> List.indexedMap viewRow
        )


stringifyRowFields row =
    { measuredGravity = Maybe.withDefault "" (Maybe.map toString row.measuredGravity)
    , measuredTemperature = Maybe.withDefault "" (Maybe.map toString row.measuredTemperature)
    , hydrometerCalibration = Maybe.withDefault "" (Maybe.map toString row.hydrometerCalibration)
    , correctedGravity = Maybe.withDefault "" (Maybe.map toString row.correctedGravity)
    }


viewRow index row =
    div
        []
        [ numberInput row.measuredGravity (NewGravity index)
        , numberInput row.measuredTemperature (NewTemperature index)
        , numberInput row.hydrometerCalibration (NewCalibration index)
        ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput default inputEvent =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.defaultValue default
        , Html.Events.onInput inputEvent
        ]
        []
