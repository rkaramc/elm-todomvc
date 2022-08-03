port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput)
import Json.Decode exposing (Value, andThen, decodeValue, fail, succeed)
import Model exposing (Model, TodoItem, modelDecoder)
import String.Case



---- PROGRAM ----


main : Program Value Model Msg
main =
    Browser.document
        { view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , init = init
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



---- MODEL ----


init : Value -> ( Model, Cmd Msg )
init flags =
    case decodeValue modelDecoder flags of
        Err _ ->
            Debug.todo "unable to decode model from localstorage!"

        Ok model ->
            ( model, Cmd.none )


type Msg
    = NoOp
    | ToggleMain
    | SetVisibility String
    | ChangeNewTodoText String
    | SubmitNewTodo
    | EditItem TodoItem
    | EditItemText TodoItem Bool String
    | ToggleItem TodoItem
    | DeleteItem TodoItem
    | ClearCompleted



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleMain ->
            ( { model
                | displayMain =
                    not model.displayMain
              }
            , Cmd.none
            )

        SetVisibility visibility ->
            ( { model
                | visibility =
                    visibility
              }
            , Cmd.none
            )

        ChangeNewTodoText newTodoText ->
            ( { model
                | newTodoText =
                    newTodoText
              }
            , Cmd.none
            )

        SubmitNewTodo ->
            ( { model
                | todoList =
                    model.todoList ++ [ TodoItem model.newTodoText False False ]
                , newTodoText =
                    ""
              }
            , Cmd.none
            )

        EditItem todo ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\a ->
                                if todo.description == a.description then
                                    { todo | editing = not todo.editing }

                                else
                                    a
                            )
              }
            , Cmd.none
            )

        EditItemText todo continueEditing desc ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\a ->
                                if todo.description == a.description then
                                    { todo
                                        | description = desc
                                        , editing = continueEditing
                                    }

                                else
                                    a
                            )
              }
            , Cmd.none
            )

        ToggleItem todo ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\a ->
                                if todo.description == a.description then
                                    { todo | completed = not todo.completed }

                                else
                                    a
                            )
              }
            , Cmd.none
            )

        DeleteItem todo ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.filter (\a -> a.description /= todo.description)
              }
            , Cmd.none
            )

        ClearCompleted ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.filter (\a -> not a.completed)
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "todoapp" ]
            [ header [ class "header " ]
                [ h1 [] [ text "todos" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done?"
                    , autofocus True
                    , value model.newTodoText
                    , onInput ChangeNewTodoText
                    , onEnter SubmitNewTodo
                    ]
                    []
                , input [ class "toggle-all", id "toggle-all", type_ "checkbox", onClick ToggleMain ] []
                , label [ for "toggle-all" ] [ text "Toggle display of all Todos" ]
                ]
            , viewTodoList model
            , viewFooterActions model
            ]
        , viewFooterInfo
        ]


viewFooterActions : Model -> Html Msg
viewFooterActions model =
    let
        itemCount =
            model.todoList |> List.length

        textCount =
            String.fromInt itemCount
                ++ (if itemCount /= 1 then
                        " items"

                    else
                        " item"
                   )

        completedItemCount =
            model.todoList |> List.filter (\a -> a.completed) |> List.length

        clearCompletedLink =
            if completedItemCount > 0 then
                [ button
                    [ class "clear-completed"
                    , onClick ClearCompleted
                    ]
                    [ text "Clear completed" ]
                ]

            else
                []

        filters =
            [ "all", "active" ]
                ++ (if completedItemCount > 0 then
                        [ "completed" ]

                    else
                        []
                   )

        filterButton filter =
            li []
                [ a
                    [ classList
                        [ ( "selected", model.visibility == filter )
                        ]
                    , onClick (SetVisibility filter)
                    ]
                    [ text (filter |> toSentenceCase) ]
                ]
    in
    if itemCount > 0 then
        footer [ class "footer" ]
            ([ span
                [ class "todo-count" ]
                [ strong [] [ text textCount ] ]
             , ul
                [ class "filters" ]
                (filters |> List.map filterButton)
             ]
                ++ clearCompletedLink
            )

    else
        span [] []


viewFooterInfo : Html Msg
viewFooterInfo =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p [] [ text "Template by ", a [ href "http://sindresorhus.com", target "_blank" ] [ text "Sindre Sorhus" ] ]
        , p [] [ text "Created by ", a [ href "http://karamch.com/about", target "_blank" ] [ text "Rajeev K" ] ]
        , p [] [ text "Part of ", a [ href "http://todomvc.com", target "_blank" ] [ text "TodoMVC" ] ]
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        todoFilter =
            case model.visibility of
                "active" ->
                    \a -> not a.completed

                "completed" ->
                    \a -> a.completed

                _ ->
                    \_ -> True
    in
    section
        [ class "main"
        , hidden (not model.displayMain)
        ]
        [ ul [ class "todo-list" ]
            (model.todoList
                |> List.filter todoFilter
                |> List.map viewTodoItem
            )
        ]


viewTodoItem : TodoItem -> Html Msg
viewTodoItem todo =
    let
        -- desc =
        --     todo.description
        handleDoubleClick =
            if not todo.editing then
                EditItem todo

            else
                NoOp
    in
    li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", todo.editing )
            ]
        , onDoubleClick handleDoubleClick
        ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (ToggleItem todo)
                ]
                []
            , label [] [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (DeleteItem todo)
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , onInput (EditItemText todo True)
            , onEnter (EditItemText todo False todo.description)
            , onBlur (EditItemText todo False todo.description)
            ]
            []
        ]



---- UTILITIES ----


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                succeed msg

            else
                fail "not ENTER"
    in
    on "keydown" (andThen isEnter keyCode)


toSentenceCase : String -> String
toSentenceCase =
    String.Case.convertCase " " True False
