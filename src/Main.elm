module Main exposing (main)

import Array as A exposing (Array)
import Browser as B
import Html exposing (Html, button, div, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List as L
import Random as R
import String as S


type Ms
    = Ms Int


type Msg
    = GetRandomMs Int
    | Load
    | Search Cell Int
    | SearchAll


type Options
    = Options (Maybe Int)


type alias IsSearching =
    Bool


type Cell
    = Cell IsSearching (Maybe Options)


type alias Model =
    { isLoaded : Bool
    , cells : List (List Cell)
    }


main : Program () Model Msg
main =
    B.element
        { init = \flags -> init flags
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


randomMs : R.Generator Int
randomMs =
    R.int 1 500


octoberDays =
    L.map (\n -> "Oct " ++ S.fromInt n) <| L.range 1 32


hours =
    L.range 0 23


initCalendar w h =
    L.repeat h <| L.repeat w <| Cell False Nothing



--search model =


initModel : Model
initModel =
    { isLoaded = False, cells = initCalendar 32 24 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, R.generate GetRandomMs randomMs )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomMs ms ->
            ( model, Cmd.none )

        Load ->
            ( { model | isLoaded = True }, Cmd.none )

        Search (Cell isSearching options) n ->
            let
                updateCell =
                    L.map (\row -> A.fromList row) model.cells

                _ =
                    Debug.log "search " updateCell
            in
            ( model, Cmd.none )

        --SearchAll ->
        --( ms, Cmd.none )
        _ ->
            ( model, Cmd.none )


subscriptions _ =
    Sub.none


headCell day =
    th [ class "day-header" ] [ text day ]


head =
    tr [] <| L.map headCell octoberDays


bodyRow hour row =
    tr [] <| L.map (\cellValue -> cell hour cellValue) row


cell hour cellValue =
    td [ class "hour-cell", onClick <| Search cellValue ] [ div [ class "time" ] [ text <| S.fromInt hour ++ ":00" ] ]


body cells =
    L.indexedMap (\hour row -> bodyRow hour row) cells


grid model =
    case model.isLoaded of
        False ->
            [ button [ class "btn", onClick Load ] [ text "Load" ] ]

        True ->
            [ button [ class "btn", onClick SearchAll ] [ text "Search all month" ]
            , table [] [ tbody [] <| head :: body model.cells ]
            ]


view model =
    div [] <| grid model
