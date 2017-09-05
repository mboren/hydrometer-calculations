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
    }


emptyRow : String -> Row
emptyRow calibration =
    { measuredGravity = ""
    , measuredTemperature = ""
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
    ( { table = []
      , tableState = Table.initialSort ""
      , lastRow = emptyRow (toString defaultCalibration)
      , defaultCalibration = defaultCalibration
      }
    , Cmd.none
    )


type Msg
    = NewGravity Int String
    | NewTemperature Int String
    | NewCalibration Int String
    | SetTableState Table.State
    | DeleteRow Int
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGravity index value ->
            ( handleInputFields (setGravity value) index model
            , Cmd.none
            )

        NewCalibration index value ->
            let
                newDefault =
                    if index == List.length model.table - 1 then
                        value |> String.toFloat |> Result.withDefault model.defaultCalibration
                    else
                        model.defaultCalibration

                newModel =
                    model
                        |> handleInputFields (setCalibration value) index
                        |> setDefaultCalibration newDefault
            in
            ( newModel
            , Cmd.none
            )

        NewTemperature index value ->
            ( handleInputFields (setTemperature value) index model
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        Clear ->
            ( { model
                | table = []
              }
            , Cmd.none
            )

        DeleteRow index ->
            ( { model
                | table = deleteRow index model.table
              }
            , Cmd.none
            )


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
                |> updateRow index updateCorrectedGravity

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
        |> List.indexedMap (,)
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
            setCalibration (toString calibration) model.lastRow
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


updateCorrectedGravity : Row -> Row
updateCorrectedGravity row =
    let
        newCorrectedGravity =
            Result.map3
                Brew.hydrometerTempCorrection
                (String.toFloat row.measuredGravity)
                (String.toFloat row.measuredTemperature)
                (String.toFloat row.hydrometerCalibration)
    in
    { row
        | correctedGravity = Result.toMaybe newCorrectedGravity
    }


updateAbv : Maybe Float -> Row -> Row
updateAbv og row =
    { row
        | abv = Maybe.map2 Brew.calculateAbv og row.correctedGravity
    }


addEmptyRow : Float -> List Row -> List Row
addEmptyRow calibration rows =
    List.append rows [ emptyRow (toString calibration) ]


formatGravity =
    Maybe.map (format "0.000") >> Maybe.withDefault ""


formatAbv =
    Maybe.map (format "0.00%") >> Maybe.withDefault ""


config lastRowIndex =
    Table.config
        { toId = Tuple.first >> toString
        , toMsg = SetTableState
        , columns =
            [ inputColumn "Measured SG" .measuredGravity NewGravity
            , inputColumn "Measured Temp (F)" .measuredTemperature NewTemperature
            , inputColumn "Hydrometer Calibration Temp (F)" .hydrometerCalibration NewCalibration
            , outputColumn "Corrected SG" (.correctedGravity >> formatGravity)
            , outputColumn "ABV" (.abv >> formatAbv)
            , deleteColumn lastRowIndex
            ]
        }


view : Model -> Html Msg
view model =
    let
        cfg =
            config (List.length model.table)

        tableData =
            List.indexedMap (,) (model.table ++ [ model.lastRow ])

        clearButton =
            if List.isEmpty model.table then
                Html.text ""
            else
                button
                    [ Html.Events.onClick Clear
                    , Html.Attributes.tabindex 1
                    ]
                    [ text "delete everything in table" ]
    in
    div
        []
        [ clearButton
        , br [] []
        , Table.view cfg model.tableState tableData
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
        [ Html.Attributes.style
            [ ( "background", "#EEEEEE" ) ]
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
