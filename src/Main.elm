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
    = StartSearch Int Int Float
    | GetResult Int Int
    | SearchDone Int Int Int
    | Load
    | Search Int Int
    | SearchAll


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


randomMs : R.Generator Float
randomMs =
    R.float 1 500


rnd : R.Generator Int
rnd =
    R.int 1 4


octoberDays =
    L.map (\n -> "Oct " ++ S.fromInt n) <| L.range 1 31


initCalendar w h =
    L.repeat h <| L.repeat w <| Cell False Nothing


initModel : Model
initModel =
    { isLoaded = False, cells = initCalendar 31 24 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


updateNth : (b -> b) -> Int -> List b -> List b
updateNth f n xs =
    L.indexedMap
        (\i x ->
            if i == n then
                f x

            else
                x
        )
        xs


updateNth2 : (b -> b) -> Int -> Int -> List (List b) -> List (List b)
updateNth2 f n k xs =
    updateNth
        (\row -> updateNth f n row)
        k
        xs


delay : Int -> Int -> Float -> Cmd Msg
delay x y time =
    Task.perform (\_ -> GetResult x y) <| P.sleep time


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


updateAll =
    let
        xs =
            L.repeat 24 <| L.range 0 30
    in
    L.concat <| L.indexedMap (\i row -> L.map (\it -> send <| Search it i) row) xs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetResult x y ->
            ( model, R.generate (SearchDone x y) rnd )

        SearchDone x y result ->
            let
                updateCell val =
                    Cell False (Just result)
            in
            ( { model | cells = updateNth2 updateCell x y model.cells }, Cmd.none )

        StartSearch x y ms ->
            ( model, delay x y ms )

        Load ->
            ( { model | isLoaded = True }, Cmd.none )

        Search x y ->
            let
                updateCell _ =
                    Cell True Nothing
            in
            ( { model | cells = updateNth2 updateCell x y model.cells }
            , R.generate (StartSearch x y) randomMs
            )

        SearchAll ->
            ( model, Cmd.batch updateAll )



--( ms, Cmd.none )
-- _ ->
--     ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


headCell : String -> Html Msg
headCell day =
    th [ class "day-header" ] [ text day ]


head =
    tr [] <| L.map headCell octoberDays


bodyRow : Int -> List Cell -> Html Msg
bodyRow hour row =
    tr [] <| L.indexedMap (\x cellValue -> cell x hour cellValue) row


resultSearch v =
    if v > 3 then
        "good-results"

    else if v > 1 && v <= 3 then
        "weak-results"

    else if v >= 0 && v <= 1 then
        "bad-results"

    else
        "error"


cell : Int -> Int -> Cell -> Html Msg
cell x hour cellValue =
    case cellValue of
        Cell False Nothing ->
            td [ class "hour-cell", onClick <| Search x hour ] [ div [ class "time" ] [ text <| S.fromInt hour ++ ":00" ] ]

        Cell True Nothing ->
            td [ class "hour-cell" ] [ div [ class "searching" ] [ text "..." ] ]

        Cell False (Just val) ->
            td [ class "hour-cell" ]
                [ div []
                    [ div [] [ text <| S.fromInt val ]
                    , div [ class <| resultSearch val ] [ text "result" ]
                    ]
                ]

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
