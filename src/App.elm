module App exposing (main)

import Brew
import Html exposing (..)
import Html.Attributes
import Html.Events
import Table


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
    { id : Int
    , measuredGravity : Maybe Float
    , measuredTemperature : Maybe Temperature
    , hydrometerCalibration : Maybe Temperature
    , correctedGravity : Maybe Float
    }


type alias Model =
    { table : List Row
    , tableState : Table.State
    }


emptyRow id =
    Row id Nothing Nothing Nothing Nothing


init : ( Model, Cmd Msg )
init =
    ( Model [ emptyRow 0 ] (Table.initialSort ""), Cmd.none )


type Msg
    = NewGravity Int String
    | NewTemperature Int String
    | NewCalibration Int String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGravity index value ->
            ( handleInputFields setGravity index value model
            , Cmd.none
            )

        NewCalibration index value ->
            ( handleInputFields setCalibration index value model
            , Cmd.none
            )

        NewTemperature index value ->
            ( handleInputFields setTemperature index value model
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


handleInputFields : (Maybe Float -> Row -> Row) -> Int -> String -> Model -> Model
handleInputFields rowUpdate index value model =
    let
        lastRow =
            index == List.length model.table - 1

        parsedValue =
            String.toFloat value |> Result.toMaybe

        newTable =
            model.table
                |> updateRow index (rowUpdate parsedValue)
                |> updateRow index updateRowCalculations
                |> (if lastRow then
                        addEmptyRow
                    else
                        identity
                   )
    in
    { model | table = newTable }


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


addEmptyRow : List Row -> List Row
addEmptyRow rows =
    List.append rows [ emptyRow (List.length rows) ]


config =
    Table.config
        { toId = .id >> toString
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Measured SG" .measuredGravity
            , Table.stringColumn "Measured Temp (F)" .measuredTemperature
            , Table.stringColumn "Hydrometer Calibration Temp (F)" .hydrometerCalibration
            , Table.stringColumn "Corrected SG" .correctedGravity
            ]
        }


view : Model -> Html Msg
view model =
    div []
        [ viewTable model.table
        , Table.view config model.tableState (model.table |> List.map stringifyRowFields)
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
    { id = row.id
    , measuredGravity = Maybe.withDefault "" (Maybe.map toString row.measuredGravity)
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
