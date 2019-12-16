module Main exposing (main)

import Basics exposing (Never, never)
import Browser as B
import Html exposing (Html, button, div, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List as L
import Process as P
import Random as R
import String as S
import Task


type Msg
    = GetRandomMs Int
    | Load
    | Search Cell Int Int
    | SearchAll


type Options
    = Options (Maybe Int)


type alias IsSearching =
    Bool


type Cell
    = Cell IsSearching (Maybe Int)


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


initCalendar w h =
    L.repeat h <| L.repeat w <| Cell False Nothing


initModel : Model
initModel =
    { isLoaded = False, cells = initCalendar 32 24 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


updateCells : Int -> Int -> List (List Cell) -> List (List Cell)
updateCells x y xs =
    let
        updateCell yy row =
            L.indexedMap
                (\xx it ->
                    if xx == x && yy == y then
                        Cell True Nothing

                    else
                        it
                )
                row
    in
    L.indexedMap (\yy row -> updateCell yy row) xs



--delay : Float -> Msg -> Cmd Msg


delay time msg =
    let
        _ =
            Debug.log "delay" 1
    in
    Task.perform (\_ -> msg) <|
        Task.andThen (always <| Task.succeed msg) <|
            P.sleep time



--P.sleep time (\_ -> Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRandomMs ms ->
            let
                _ =
                    Debug.log "delay " ms
            in
            ( model, Cmd.none )

        Load ->
            ( { model | isLoaded = True }, Cmd.none )

        Search (Cell isSearching options) x y ->
            let
                updateCell =
                    updateCells x y model.cells
            in
            ( { model | cells = updateCell }, delay 4000 msg )

        --SearchAll ->
        --( ms, Cmd.none )
        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


headCell day =
    th [ class "day-header" ] [ text day ]


head =
    tr [] <| L.map headCell octoberDays


bodyRow : Int -> List Cell -> Html Msg
bodyRow hour row =
    tr [] <| L.indexedMap (\x cellValue -> cell x hour cellValue) row


cell : Int -> Int -> Cell -> Html Msg
cell x hour cellValue =
    case cellValue of
        Cell False Nothing ->
            td [ class "hour-cell", onClick <| Search cellValue x hour ] [ div [ class "time" ] [ text <| S.fromInt hour ++ ":00" ] ]

        Cell True Nothing ->
            td [ class "hour-cell" ] [ div [ class "searching" ] [ text "..." ] ]

        _ ->
            td [] [ text "else" ]



--case isSearching of
--    True ->
--        td [ class "hour-cell" ] [ div [ class "searching" ] [ text "..." ] ]
--
--    False ->
--        td [ class "hour-cell", onClick <| Search cellValue x hour ] [ div [ class "time" ] [ text <| S.fromInt hour ++ ":00" ] ]


body : List (List Cell) -> List (Html Msg)
body cells =
    L.indexedMap (\hour row -> bodyRow hour row) cells


grid model =
    case model.isLoaded of
        False ->
            [ button [ class "btn", onClick Load ] [ text "Load" ] ]

        True ->
            [ button [ class "btn", onClick SearchAll ] [ text "Search all month" ]
            , table []
                [ tbody [] <| head :: body model.cells
                ]
            ]


view model =
    div [] <| grid model
