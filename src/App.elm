module App exposing (main)

import Brew
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
    let
        ( rowUpdate, index, value ) =
            case msg of
                NewGravity index value ->
                    ( setGravity, index, value )

                NewCalibration index value ->
                    ( setCalibration, index, value )

                NewTemperature index value ->
                    ( setTemperature, index, value )

        parsedValue =
            String.toFloat value |> Result.toMaybe

        newTable =
            model.table
                |> updateRow index (rowUpdate parsedValue)
                |> updateRow index updateRowCalculations
    in
    ( { model | table = newTable }, Cmd.none )


setGravity : Maybe Float -> Row -> Row
setGravity maybeGravity row =
    { row | measuredGravity = maybeGravity }


setTemperature : Maybe Temperature -> Row -> Row
setTemperature maybeTemperature row =
    { row | measuredTemperature = maybeTemperature }


setCalibration : Maybe Temperature -> Row -> Row
setCalibration maybeCalibration row =
    { row | hydrometerCalibration = maybeCalibration }


setCorrectedGravity : Maybe Float -> Row -> Row
setCorrectedGravity maybeGravity row =
    { row | correctedGravity = maybeGravity }


updateRow : Int -> (Row -> Row) -> List Row -> List Row
updateRow index f table =
    let
        updateFunc i row =
            if i == index then
                f row
            else
                row
    in
    List.indexedMap updateFunc table


updateRowCalculations : Row -> Row
updateRowCalculations row =
    let
        newCorrectedGravity =
            case ( row.measuredGravity, row.measuredTemperature, row.hydrometerCalibration ) of
                ( Just gravity, Just temp, Just calibration ) ->
                    Just (Brew.hydrometerTempCorrection gravity temp calibration)

                _ ->
                    Nothing
    in
    { row | correctedGravity = newCorrectedGravity }


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
        , text row.correctedGravity
        ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput default inputEvent =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.defaultValue default
        , Html.Events.onInput inputEvent
        ]
        []
