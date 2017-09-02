module App exposing (main)

import Brew
import Html exposing (..)
import Html.Attributes
import Html.Events
import Numeral exposing (format)
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
    , abv : Maybe Float
    }


type alias Model =
    { table : List Row
    , tableState : Table.State
    , defaultCalibration : Float
    }


emptyRow id calibration =
    { id = id
    , measuredGravity = Nothing
    , measuredTemperature = Nothing
    , hydrometerCalibration = calibration
    , correctedGravity = Nothing
    , abv = Nothing
    }


init : ( Model, Cmd Msg )
init =
    let
        defaultCalibration =
            60
    in
    ( Model [ emptyRow 0 (Just defaultCalibration) ] (Table.initialSort "") defaultCalibration, Cmd.none )


type Msg
    = NewGravity Int String
    | NewTemperature Int String
    | NewCalibration Int String
    | SetTableState Table.State
    | NewDefaultCalibration String


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

        NewDefaultCalibration value ->
            let
                parsedValue =
                    String.toFloat value

                newDefault =
                    case parsedValue of
                        Ok val ->
                            val

                        Err _ ->
                            model.defaultCalibration
            in
            ( { model | defaultCalibration = newDefault }
            , Cmd.none
            )


handleInputFields : (Maybe Float -> Row -> Row) -> Int -> String -> Model -> Model
handleInputFields rowUpdate index value model =
    let
        lastRow =
            index == List.length model.table - 1

        parsedValue =
            String.toFloat value |> Result.toMaybe

        tableWithUpdatedGravity =
            model.table
                |> updateRow index (rowUpdate parsedValue)
                |> updateRow index updateCorrectedGravity
                |> (if lastRow then
                        addEmptyRow (Just model.defaultCalibration)
                    else
                        identity
                   )

        og =
            tableWithUpdatedGravity
                |> List.head
                |> Maybe.andThen .correctedGravity

        -- This recalculates ABVs for the whole table every time
        -- any value is updated. It really only needs to update
        -- every row when the first row changes, however, this
        -- makes the logic simpler and i really don't expect
        -- performance issues with this app.
        tableWithUpdatedAbvs =
            tableWithUpdatedGravity
                |> List.map (updateAbv og)
    in
    { model | table = tableWithUpdatedAbvs }


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
        updateFunc row =
            if row.id == index then
                f row
            else
                row
    in
    List.map updateFunc table


updateCorrectedGravity : Row -> Row
updateCorrectedGravity row =
    let
        newCorrectedGravity =
            Maybe.map3
                Brew.hydrometerTempCorrection
                row.measuredGravity
                row.measuredTemperature
                row.hydrometerCalibration
    in
    { row | correctedGravity = newCorrectedGravity }


updateAbv : Maybe Float -> Row -> Row
updateAbv og row =
    { row | abv = Maybe.map2 Brew.calculateAbv og row.correctedGravity }


addEmptyRow : Maybe Float -> List Row -> List Row
addEmptyRow calibration rows =
    List.append rows [ emptyRow (List.length rows) calibration ]


maybeToString =
    Maybe.map toString >> Maybe.withDefault ""


formatGravity =
    Maybe.map (format "0.000") >> Maybe.withDefault ""


formatAbv =
    Maybe.map (format "0.00%") >> Maybe.withDefault ""


config =
    Table.config
        { toId = .id >> toString
        , toMsg = SetTableState
        , columns =
            [ inputColumn "Measured SG" (.measuredGravity >> maybeToString) NewGravity
            , inputColumn "Measured Temp (F)" (.measuredTemperature >> maybeToString) NewTemperature
            , inputColumn "Hydrometer Calibration Temp (F)" (.hydrometerCalibration >> maybeToString) NewCalibration
            , unsortableColumn "Corrected SG" (.correctedGravity >> formatGravity)
            , unsortableColumn "ABV" (.abv >> formatAbv)
            ]
        }


view : Model -> Html Msg
view model =
    div
        []
        [ label []
            [ text "Default hydrometer calibration (F) "
            , numberInput (toString model.defaultCalibration) NewDefaultCalibration
            ]
        , Table.view config model.tableState model.table
        ]

unsortableColumn name viewData =
    Table.customColumn
        { name = name
        , viewData = viewData
        , sorter = Table.unsortable
        }

inputColumn : String -> (Row -> String) -> (Int -> (String -> Msg)) -> Table.Column Row Msg
inputColumn name getValue msg =
    Table.veryCustomColumn
        { name = name, viewData = viewInputColumn getValue (\r -> msg r.id), sorter = Table.unsortable }


viewInputColumn : (Row -> String) -> (Row -> (String -> Msg)) -> Row -> Table.HtmlDetails Msg
viewInputColumn getValue getMsg row =
    Table.HtmlDetails []
        [ numberInput (getValue row) (getMsg row)
        ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput default inputEvent =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.defaultValue default
        , Html.Events.onInput inputEvent
        ]
        []
