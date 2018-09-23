module App exposing (main)

import Brew
import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Numeral exposing (format)
import Table


type TempUnit
    = Fahrenheit
    | Celcius


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Row =
    { measuredGravity : String
    , measuredTemperature : String
    , hydrometerCalibration : String
    , correctedGravity : Maybe Float
    , abv : Maybe Float
    }


type alias Model =
    { table : List Row
    , lastRow : Row
    , tableState : Table.State
    , defaultCalibration : Float
    , tempUnit : TempUnit
    }


emptyRow : String -> Row
emptyRow calibration =
    { measuredGravity = ""
    , measuredTemperature = ""
    , hydrometerCalibration = calibration
    , correctedGravity = Nothing
    , abv = Nothing
    }


init : Model
init =
    let
        defaultCalibration =
            60
    in
    { table = []
    , tableState = Table.initialSort ""
    , lastRow = emptyRow (String.fromFloat defaultCalibration)
    , defaultCalibration = defaultCalibration
    , tempUnit = Fahrenheit
    }


type Msg
    = NewGravity Int String
    | NewTemperature Int String
    | NewCalibration Int String
    | SwitchTemperatureUnit
    | SetTableState Table.State
    | DeleteRow Int
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGravity index value ->
            handleInputFields (setGravity value) index model

        NewCalibration index value ->
            let
                newDefault =
                    if index == List.length model.table - 1 then
                        value |> String.toFloat |> Maybe.withDefault model.defaultCalibration

                    else
                        model.defaultCalibration

                newModel =
                    model
                        |> handleInputFields (setCalibration value) index
                        |> setDefaultCalibration newDefault
            in
            newModel

        NewTemperature index value ->
            handleInputFields (setTemperature value) index model

        SetTableState newState ->
            { model | tableState = newState }

        Clear ->
            { model
                | table = []
            }

        DeleteRow index ->
            { model
                | table = deleteRow index model.table
            }

        SwitchTemperatureUnit ->
            let
                unit =
                    case model.tempUnit of
                        Fahrenheit ->
                            Celcius

                        Celcius ->
                            Fahrenheit

                nt =
                    model.table
                        |> List.map (updateCorrectedGravity unit)
                        |> updateTableAbvs
            in
            { model | tempUnit = unit, table = nt }


{-| Update table when an input field changes.
This includes:

  - Setting input field value
  - recalculating corrected SG for changed row
  - recalculating ABV for the whole table
  - adding an empty row to the table if changed row is last row

-}
handleInputFields : (Row -> Row) -> Int -> Model -> Model
handleInputFields rowUpdate index model =
    let
        lastRow =
            index == List.length model.table

        tableWithUpdatedGravity =
            model.table
                |> (if lastRow then
                        addEmptyRow model.defaultCalibration

                    else
                        identity
                   )
                |> updateRow index rowUpdate
                |> updateRow index (updateCorrectedGravity model.tempUnit)

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
            List.map (updateAbv og) tableWithUpdatedGravity
    in
    { model | table = tableWithUpdatedAbvs }


{-| Delete row from table and update table ABV values if it was the first row
-}
deleteRow : Int -> List Row -> List Row
deleteRow index rows =
    rows
        |> List.indexedMap (\a b -> ( a, b ))
        |> List.filter (Tuple.first >> (/=) index)
        |> List.map Tuple.second
        |> (if index == 0 then
                updateTableAbvs

            else
                identity
           )


updateTableAbvs : List Row -> List Row
updateTableAbvs table =
    let
        og =
            table
                |> List.head
                |> Maybe.andThen .correctedGravity
    in
    List.map (updateAbv og) table


setDefaultCalibration : Float -> Model -> Model
setDefaultCalibration calibration model =
    let
        newLastRow =
            setCalibration (String.fromFloat calibration) model.lastRow
    in
    { model
        | defaultCalibration = calibration
        , lastRow = newLastRow
    }


setGravity : String -> Row -> Row
setGravity gravity row =
    { row | measuredGravity = gravity }


setTemperature : String -> Row -> Row
setTemperature temperature row =
    { row | measuredTemperature = temperature }


setCalibration : String -> Row -> Row
setCalibration calibration row =
    { row | hydrometerCalibration = calibration }


setCorrectedGravity : Maybe Float -> Row -> Row
setCorrectedGravity gravity row =
    { row | correctedGravity = gravity }


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


updateCorrectedGravity : TempUnit -> Row -> Row
updateCorrectedGravity unit row =
    let
        convert : String -> Maybe Float
        convert s =
            case unit of
                Fahrenheit ->
                    String.toFloat s

                Celcius ->
                    s
                        |> String.toFloat
                        |> Maybe.map celciusToFahrenheit

        newCorrectedGravity =
            Maybe.map3
                Brew.hydrometerTempCorrection
                (String.toFloat row.measuredGravity)
                (convert row.measuredTemperature)
                (convert row.hydrometerCalibration)
    in
    { row
        | correctedGravity = newCorrectedGravity
    }


updateAbv : Maybe Float -> Row -> Row
updateAbv og row =
    { row
        | abv = Maybe.map2 Brew.calculateAbv og row.correctedGravity
    }


addEmptyRow : Float -> List Row -> List Row
addEmptyRow calibration rows =
    List.append rows [ emptyRow (String.fromFloat calibration) ]


formatGravity =
    Maybe.map (format "0.000") >> Maybe.withDefault ""


formatAbv =
    Maybe.map (format "0.00%") >> Maybe.withDefault ""


config lastRowIndex unit =
    Table.config
        { toId = Tuple.first >> String.fromInt
        , toMsg = SetTableState
        , columns =
            [ inputColumn "Measured SG" .measuredGravity NewGravity
            , inputColumn ("Measured Temp (" ++ abbreviateUnit unit ++ ")") .measuredTemperature NewTemperature
            , inputColumn ("Hydrometer Calibration (" ++ abbreviateUnit unit ++ ")") .hydrometerCalibration NewCalibration
            , outputColumn "Corrected SG" (.correctedGravity >> formatGravity)
            , outputColumn "ABV" (.abv >> formatAbv)
            , deleteColumn lastRowIndex
            ]
        }


view : Model -> Html Msg
view model =
    let
        cfg =
            config (List.length model.table) model.tempUnit

        tableData =
            List.indexedMap (\a b -> ( a, b )) (model.table ++ [ model.lastRow ])

        clearButton =
            if List.isEmpty model.table then
                Html.text ""

            else
                button
                    [ Html.Events.onClick Clear
                    , Html.Attributes.tabindex 1
                    ]
                    [ text "delete everything in table" ]

        unitSwapButton =
            button
                [ Html.Events.onClick SwitchTemperatureUnit
                , Html.Attributes.tabindex 1
                ]
                [ text "switch temperature unit" ]
    in
    div
        []
        [ div
            []
            [ clearButton
            , unitSwapButton
            ]
        , form
            []
            [ Table.view cfg model.tableState tableData ]
        ]


inputColumn : String -> (Row -> String) -> (Int -> String -> Msg) -> Table.Column ( Int, Row ) Msg
inputColumn name getValue msg =
    Table.veryCustomColumn
        { name = name
        , viewData = viewInputColumn getValue msg
        , sorter = Table.unsortable
        }


viewInputColumn : (Row -> String) -> (Int -> String -> Msg) -> ( Int, Row ) -> Table.HtmlDetails Msg
viewInputColumn getValue getMsg ( id, row ) =
    Table.HtmlDetails []
        [ numberInput (getValue row) (getMsg id)
        ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput default inputEvent =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.value default
        , Html.Events.onInput inputEvent
        , Html.Attributes.tabindex 1
        , Html.Attributes.style "width" "98%"
        ]
        []


outputColumn : String -> (Row -> String) -> Table.Column ( Int, Row ) Msg
outputColumn name getValue =
    Table.veryCustomColumn
        { name = name
        , viewData = viewOutputColumn getValue
        , sorter = Table.unsortable
        }


viewOutputColumn : (Row -> String) -> ( a, Row ) -> Table.HtmlDetails Msg
viewOutputColumn getValue ( _, row ) =
    Table.HtmlDetails
        [ Html.Attributes.style "background" "#EEEEEE"
        ]
        [ Html.text (getValue row)
        ]


deleteColumn : Int -> Table.Column ( Int, Row ) Msg
deleteColumn lastRowIndex =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewDeleteColumn lastRowIndex
        , sorter = Table.unsortable
        }


viewDeleteColumn : Int -> ( Int, a ) -> Table.HtmlDetails Msg
viewDeleteColumn lastRowIndex ( id, _ ) =
    Table.HtmlDetails
        []
        (if lastRowIndex /= id then
            [ button
                [ Html.Events.onClick (DeleteRow id) ]
                [ text "✖" ]
            ]

         else
            []
        )


celciusToFahrenheit : Float -> Float
celciusToFahrenheit celcius =
    32 + celcius * 9 / 5


abbreviateUnit : TempUnit -> String
abbreviateUnit unit =
    case unit of
        Fahrenheit ->
            "F"

        Celcius ->
            "C"
