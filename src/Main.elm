module Main exposing (main)

import Browser
import Html exposing (Html, div, text, img)
import Html.Attributes exposing (style)
import Time exposing (Posix, Zone)
import Date
import Task

-- MODEL

type alias Model =
    { currentName : String }


-- A list of names that we pick from based on the week number.
names : List String
names =
    [ "HC"
    , "Christian"
    , "Eivind"
    , "Jonas"
    ]


-- INIT

type Msg
    = GotTime Zone Posix


init : () -> ( Model, Cmd Msg )
init _ =
    -- We request the current time at initialization
    ( { currentName = "Loading..." }
    , getCurrentTime
    )


getCurrentTime : Cmd Msg
getCurrentTime =
    let
        timeZone = Time.here -- get the local timezone
        currentTime = Time.now
    in
    Task.perform identity (Task.map2 GotTime timeZone currentTime)


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime zone posix ->
            let
                weekNumber = (Date.weekNumber (Date.fromPosix zone posix))
                chosenName =
                    let
                        index = (modBy (List.length names) weekNumber)
                    in
                    List.head (List.drop index names)
                        |> Maybe.withDefault "No name found"
            in
            ( { model | currentName = chosenName }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div
        [ style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "margin" "0"
        , style "padding" "0"
        ]
        [ -- Top Bar
          div
            [ style "background-color" "#eee"
            , style "padding" "1em"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "font-family" "sans-serif"
            , style "font-size" "1em"
            , style "font-weight" "bold"
            ]
            [ img
                [ Html.Attributes.src "/img/gyldendal.png"
                , style "height" "32px"
                , style "margin-right" "1em"
                ]
                []
            , text "Spiroobar"
            ]

          -- Main content area
        , div
            [ style "flex" "1"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "font-size" "3em"
            , style "font-family" "sans-serif"
            ]
            [ text model.currentName ]
        ]

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
