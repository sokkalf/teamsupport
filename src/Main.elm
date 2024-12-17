module Main exposing (main)

import Browser
import Date
import Html exposing (Html, div, img, input, label, strong, text, span, p)
import Html.Attributes exposing (class, checked, style, type_)
import Html.Events exposing (onCheck)
import Task
import Time exposing (Posix, Zone)


-- MODEL

type alias Model =
    { currentName : String
    , allNames : List String
    , showAll : Bool
    }


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
    | ToggleShowAll Bool


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentName = "Loading...", allNames = names, showAll = False }
    , getCurrentTime
    )


getCurrentTime : Cmd Msg
getCurrentTime =
    let
        timeZone =
            Time.here

        currentTime =
            Time.now
    in
    Task.perform identity (Task.map2 GotTime timeZone currentTime)


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime zone posix ->
            let
                weekNumber =
                    Date.weekNumber (Date.fromPosix zone posix)

                chosenName =
                    let
                        index =
                            modBy (List.length names) weekNumber
                    in
                    List.head (List.drop index names)
                        |> Maybe.withDefault "No name found"
            in
            ( { model | currentName = chosenName }, Cmd.none )

        ToggleShowAll newVal ->
            ( { model | showAll = newVal }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div
        [ style "height" "100em"
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
            , style "justify-content" "space-between"
            , style "font-family" "sans-serif"
            , style "font-size" "1em"
            , style "font-weight" "bold"
            , class "top-bar"
            ]
            [ div
                [ style "display" "flex"
                , style "align-items" "center"
                ]
                [ img
                    [ Html.Attributes.src "img/gyldendal.png"
                    , style "height" "32px"
                    , style "margin-right" "1em"
                    ]
                    []
                , text "Spiroobar"
                ]
            , div [] [
                span [ style "padding-right" "0.75em" ] [ text "Eivind mode" ]
                , label [ class "switch" ]
                [ input
                    [ type_ "checkbox"
                    , checked model.showAll
                    , onCheck ToggleShowAll
                    ]
                    []
                , span [ class "slider round" ] []
                ]
            ]]

        -- Main content area
        , div
            [ style "flex" "1"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "font-size" "3em"
            , style "font-family" "sans-serif"
            ]
            [ if model.showAll then
                viewAllNames model
              else
                text model.currentName
            ]
        ]


viewAllNames : Model -> Html Msg
viewAllNames model =
    let
        -- Helper function to turn each name into either bold or normal text
        nameToHtml name =
            if name == model.currentName then
                strong [] [ text name ]
            else
                text name

        -- Build the list of names separated by ", "
        namesWithSeparators : List (Html Msg)
        namesWithSeparators =
            case model.allNames of
                [] ->
                    []

                first :: rest ->
                    nameToHtml first
                        :: List.concatMap (\n -> [ text ", ", nameToHtml n ]) rest
    in
    div [] namesWithSeparators


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
