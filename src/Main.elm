module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
  {}


init : ( Model, Cmd Msg )
init =
  ( {}, Cmd.none )



---- UPDATE ----


type Msg
  = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
  ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view _ =
  section [ class "todoapp"]
    [ header [ class "header "]
        [ h1 [] [ text "todos" ]
        , input [ class "new-todo", placeholder "What needs to be done?", autofocus True ][]
        ]
    , section [ class "main" ]
        [ input [ class "toggle-all", id "toggle-all", type_ "checkbox" ] []
        , label [ for "toggle-all"] [ text "Mark all as complete" ]
        , ul [ class "todo-list" ]
            [ li [ class "completed" ]
                [ div [ class "view" ]
                  [ input [ class "toggle", type_ "checkbox", checked True] []
                  , label [] [text "Taste JavaScript" ]
                  , button [ class "destroy" ] []
                  ]
                , input [ class "edit", value "Create a TodoMVC template" ] []
                ]
            , li []
                [ div [ class "view" ]
                  [ input [ class "toggle", type_ "checkbox", checked False] []
                  , label [] [text "Buy a unicorn" ]
                  , button [ class "destroy" ] []
                  ]
                , input [ class "edit", value "Rule the web" ] []
                ]
            ]
        ]
    , footer [ class "footer" ]
        [ span [ class "todo-count" ] [ strong [] [ text "0" ], text " item left" ]
        , ul [ class "filters" ]
            [ li [] [ a [ class "selected", href "#/" ] [ text "All" ] ]
            , li [] [ a [ href "#/active" ] [ text "Active" ] ]
            , li [] [ a [ href "#/completed" ] [ text "Completed" ] ]
            ]
        , button [ class "clear-completed" ] [ text "Clear completed" ]
        ]
    , footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p [] [ text "Template by ", a [ href "http://sindresorhus.com" ] [ text "Sindre Sorhus" ]]
        , p [] [ text "Created by ", a [ href "http://karamch.com/about" ] [ text "Rajeev K" ]]
        , p [] [ text "Part of ", a [ href "http://todomvc.com" ] [ text "TodoMVC" ]]
        ]
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
