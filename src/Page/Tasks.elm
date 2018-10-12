port module Page.Tasks exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Browser
import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, field, map, string)
import Task
import Timer


type alias Id =
    Int


type alias Task =
    { id : Id
    , description : String
    , done : Bool
    , isActive : Bool
    }


type alias Model =
    { tasks : List Task
    , newTask : Maybe Task
    , lastNewId : Id
    , selectedTaskId : Id
    , timer : Timer.Model
    }


initialModel : Model
initialModel =
    { tasks = []
    , newTask = Nothing
    , lastNewId = 0
    , selectedTaskId = 0
    , timer = Timer.init
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


viewNewTask : Model -> Html Msg
viewNewTask model =
    case model.newTask of
        Nothing ->
            div [ class "new-task d-flex justify-content-center" ]
                [ a
                    [ href "#"
                    , id "new-task-btn"
                    , onClick (UpdateTask "")
                    ]
                    [ i [ class "fa fa-plus" ] []
                    ]
                ]

        Just task ->
            div [ class "new-task" ]
                [ form [ class "d-flex", onSubmit SaveTask ]
                    [ input
                        [ type_ "text"
                        , class "flex-fill"
                        , id "new-task-description"
                        , placeholder "Add a task..."
                        , value task.description
                        , onInput UpdateTask
                        ]
                        []
                    , button
                        [ disabled (String.isEmpty task.description)
                        , class "btn"
                        ]
                        [ i [ class "fa fa-check" ]
                            []
                        ]
                    ]
                ]


viewStartTaskButton : Task -> Html Msg
viewStartTaskButton task =
    button
        [ class "btn start-task-btn"
        , onClick (ToggleTimer task)
        ]
        [ i [ class "fa fa-play" ] []
        ]


viewRemoveTaskButton : Task -> Html Msg
viewRemoveTaskButton task =
    button
        [ class "btn remove-task-btn"
        , onClick (RemoveTask task)
        ]
        [ i [ class "fa fa-remove" ] []
        ]


viewStopTaskButton : Task -> Html Msg
viewStopTaskButton task =
    button
        [ class "btn stop-task-btn"
        , onClick (ToggleTimer task)
        ]
        [ i [ class "fa fa-stop" ] []
        ]


viewTaskActions : Model -> Task -> Html Msg
viewTaskActions model task =
    let
        buttons =
            if task.isActive then
                if model.timer.isActive then
                    [ viewStopTaskButton task ]

                else
                    [ viewStartTaskButton task ]

            else if model.timer.isActive then
                [ viewRemoveTaskButton task ]

            else
                [ viewStartTaskButton task
                , viewRemoveTaskButton task
                ]
    in
    div [ class "actions d-flex justify-content-center align-items-center" ]
        buttons


viewTask : Model -> Task -> Html Msg
viewTask model task =
    li
        [ classList
            [ ( "task d-flex", True )
            , ( "selected", model.selectedTaskId == task.id )
            , ( "active", task.isActive )
            ]
        ]
        [ div
            [ class "desc flex-fill"
            , onDoubleClick (ToggleTimer task)
            , onClick (SelectTask task)
            ]
            [ text task.description ]
        , viewTaskActions model task
        ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    ul [ class "tasks" ] (List.map (viewTask model) model.tasks)


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ Timer.view model.timer |> Html.map TimerMsg
        , viewTaskList model
        , viewNewTask model
        ]


type Msg
    = SaveTask
    | UpdateTask String
    | RemoveTask Task
    | ToggleTimer Task
    | SelectTask Task
    | FocusResult (Result Browser.Dom.Error ())
    | OnKeyPressed String
    | TimerMsg Timer.Msg


getNewTaskId : Model -> Id
getNewTaskId model =
    model.lastNewId + 1


addNewTask : Model -> Model
addNewTask model =
    case model.newTask of
        Nothing ->
            model

        Just task ->
            let
                newTaskId =
                    getNewTaskId model

                newTask =
                    { task | id = newTaskId }

                newTasks =
                    model.tasks ++ [ newTask ]
            in
            { model
                | tasks = newTasks
                , lastNewId = newTaskId
                , selectedTaskId = newTaskId
                , newTask = Nothing
            }


removeTask : Model -> Task -> Model
removeTask model taskToRemove =
    let
        isNotRemovable task =
            task.id /= taskToRemove.id
    in
    { model | tasks = List.filter isNotRemovable model.tasks }


send : Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity


getNextTaskId : List Task -> Id -> Maybe Id
getNextTaskId tasks selectedTaskId =
    case tasks of
        [] ->
            Nothing

        task :: rest ->
            if task.id == selectedTaskId then
                let
                    maybeNextTask =
                        List.head rest
                in
                case maybeNextTask of
                    Nothing ->
                        Nothing

                    Just nextTask ->
                        Just nextTask.id

            else
                getNextTaskId rest selectedTaskId


getFirstTaskId : List Task -> Id
getFirstTaskId tasks =
    let
        maybeFirstTask =
            List.head tasks
    in
    case maybeFirstTask of
        Just firstTask ->
            firstTask.id

        Nothing ->
            0


selectNextTask : Model -> Model
selectNextTask model =
    let
        maybeNextTaskId =
            getNextTaskId model.tasks model.selectedTaskId

        selectedTaskId =
            case maybeNextTaskId of
                Just id ->
                    id

                Nothing ->
                    getFirstTaskId model.tasks
    in
    { model | selectedTaskId = selectedTaskId }


selectPreviousTask : Model -> Model
selectPreviousTask model =
    let
        reversedTaskList =
            List.reverse model.tasks

        maybePreviousTaskId =
            getNextTaskId reversedTaskList model.selectedTaskId

        selectedTaskId =
            case maybePreviousTaskId of
                Just id ->
                    id

                Nothing ->
                    getFirstTaskId reversedTaskList
    in
    { model | selectedTaskId = selectedTaskId }


removeSelectedTask : Model -> Maybe Task -> Model
removeSelectedTask model maybeSelectedTask =
    case maybeSelectedTask of
        Nothing ->
            model

        Just selectedTask ->
            if selectedTask.isActive then
                model

            else
                removeTask model selectedTask


startTimer : Model -> Task -> ( Model, Cmd Msg )
startTimer model task =
    let
        ( newTimer, timerCmd ) =
            Timer.shift model.timer

        updateActiveTask aTask =
            { aTask | isActive = aTask.id == task.id }

        newTasks =
            List.map updateActiveTask model.tasks
    in
    ( { model
        | timer = newTimer
        , tasks = newTasks
      }
    , Cmd.map TimerMsg timerCmd
    )


resumeTimer : Model -> ( Model, Cmd Msg )
resumeTimer model =
    ( { model
        | timer = Timer.resume model.timer
      }
    , Cmd.none
    )


stopTimer : Model -> ( Model, Cmd Msg )
stopTimer model =
    ( { model
        | timer = Timer.stop model.timer
      }
    , Cmd.none
    )


toggleTimerForSelectedTask : Model -> Maybe Task -> ( Model, Cmd Msg )
toggleTimerForSelectedTask model maybeSelectedTask =
    case maybeSelectedTask of
        Nothing ->
            ( model, Cmd.none )

        Just selectedTask ->
            if selectedTask.isActive then
                if model.timer.isActive then
                    stopTimer model

                else
                    resumeTimer model

            else if model.timer.isActive then
                ( model, Cmd.none )

            else
                startTimer model selectedTask


processKey : Model -> String -> ( Model, Cmd Msg )
processKey model key =
    let
        isSelected task =
            task.id == model.selectedTaskId

        maybeSelectedTask =
            List.filter isSelected model.tasks |> List.head
    in
    case key of
        "n" ->
            ( model, send (UpdateTask "") )

        "j" ->
            ( selectNextTask model, Cmd.none )

        "k" ->
            ( selectPreviousTask model, Cmd.none )

        "Enter" ->
            toggleTimerForSelectedTask model maybeSelectedTask

        "x" ->
            ( removeSelectedTask model maybeSelectedTask, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveTask ->
            ( addNewTask model, Cmd.none )

        UpdateTask "" ->
            let
                task =
                    Task 0 "" False False
            in
            ( { model | newTask = Just task }
            , Task.attempt FocusResult (focus "new-task-description")
            )

        FocusResult _ ->
            ( model, Cmd.none )

        UpdateTask description ->
            let
                task =
                    Task 0 description False False
            in
            ( { model | newTask = Just task }
            , Cmd.none
            )

        RemoveTask task ->
            ( removeTask model task, Cmd.none )

        ToggleTimer task ->
            toggleTimerForSelectedTask model (Just task)

        SelectTask task ->
            ( { model | selectedTaskId = task.id }, Cmd.none )

        OnKeyPressed key ->
            case model.newTask of
                Nothing ->
                    processKey model key

                Just _ ->
                    ( model, Cmd.none )

        TimerMsg timerMsg ->
            let
                ( newTimer, timerCmd ) =
                    Timer.update timerMsg model.timer
            in
            ( { model | timer = newTimer }
            , Cmd.map TimerMsg timerCmd
            )


keyDecoder : Decoder String
keyDecoder =
    field "key" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyPress <| map OnKeyPressed keyDecoder
        , Sub.map TimerMsg (Timer.subscriptions model.timer)
        ]
