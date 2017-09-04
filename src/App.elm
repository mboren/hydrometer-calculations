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
    , measuredGravity : String
    , measuredTemperature : String
    , hydrometerCalibration : String
    , correctedGravity : Maybe Float
    , abv : Maybe Float
    }


type alias Model =
    { table : List Row
    , tableState : Table.State
    , defaultCalibration : Float
    }


emptyRow : Int -> String -> Row
emptyRow id calibration =
    { id = id
    , measuredGravity = ""
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
    ( Model [ emptyRow 0 (toString defaultCalibration) ] (Table.initialSort "") defaultCalibration, Cmd.none )


type Msg
    = NewGravity Int String
    | NewTemperature Int String
    | NewCalibration Int String
    | SetTableState Table.State
    | NewDefaultCalibration String
    | Clear


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

        Clear ->
            ( { model | table = addEmptyRow model.defaultCalibration [] }
            , Cmd.none
            )


handleInputFields : (String -> Row -> Row) -> Int -> String -> Model -> Model
handleInputFields rowUpdate index value model =
    let
        lastRow =
            index == List.length model.table - 1

        tableWithUpdatedGravity =
            model.table
                |> updateRow index (rowUpdate value)
                |> updateRow index updateCorrectedGravity
                |> (if lastRow then
                        addEmptyRow model.defaultCalibration
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
        updateFunc row =
            if row.id == index then
                f row
            else
                row
    in
    List.map updateFunc table


stringToMaybeFloat =
    String.toFloat >> Result.toMaybe


updateCorrectedGravity : Row -> Row
updateCorrectedGravity row =
    let
        newCorrectedGravity =
            Maybe.map3
                Brew.hydrometerTempCorrection
                (stringToMaybeFloat row.measuredGravity)
                (stringToMaybeFloat row.measuredTemperature)
                (stringToMaybeFloat row.hydrometerCalibration)
    in
    { row | correctedGravity = newCorrectedGravity }


updateAbv : Maybe Float -> Row -> Row
updateAbv og row =
    { row | abv = Maybe.map2 Brew.calculateAbv og row.correctedGravity }


addEmptyRow : Float -> List Row -> List Row
addEmptyRow calibration rows =
    List.append rows [ emptyRow (List.length rows) (toString calibration) ]


formatGravity =
    Maybe.map (format "0.000") >> Maybe.withDefault ""


formatAbv =
    Maybe.map (format "0.00%") >> Maybe.withDefault ""


config =
    Table.config
        { toId = .id >> toString
        , toMsg = SetTableState
        , columns =
            [ inputColumn "Measured SG" .measuredGravity NewGravity
            , inputColumn "Measured Temp (F)" .measuredTemperature NewTemperature
            , inputColumn "Hydrometer Calibration Temp (F)" .hydrometerCalibration NewCalibration
            , outputColumn "Corrected SG" (.correctedGravity >> formatGravity)
            , outputColumn "ABV" (.abv >> formatAbv)
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
        , br [] []
        , button
            [ Html.Events.onClick Clear ]
            [ text "delete everything in table" ]
        , br [] []
        , Table.view config model.tableState model.table
        ]


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
        , Html.Attributes.value default
        , Html.Events.onInput inputEvent
        ]
        []


outputColumn : String -> (Row -> String) -> Table.Column Row Msg
outputColumn name getValue =
    Table.veryCustomColumn
        { name = name
        , viewData = viewOutputColumn getValue
        , sorter = Table.unsortable
        }


viewOutputColumn : (Row -> String) -> Row -> Table.HtmlDetails Msg
viewOutputColumn getValue row =
    Table.HtmlDetails
        [ Html.Attributes.style
            [ ( "background", "#EEEEEE" ) ]
        ]
        [ Html.text (getValue row)
        ]
