port module Pomodoro exposing (main)

import Browser
import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, field, map, string)
import ProgressRing
import Task
import Time


port notify : String -> Cmd msg


type alias Id =
    Int


type alias Task =
    { id : Id
    , description : String
    , done : Bool
    }


type TimerStatus
    = Work Int
    | Pause Int
    | LongPause


type alias Timer =
    { task : Task
    , initialValue : Int
    , timeout : Int
    , status : TimerStatus
    , statusCount : Int
    }


type alias Model =
    { tasks : List Task
    , newTask : Maybe Task
    , lastNewId : Id
    , selectedTaskId : Id
    , timer : Maybe Timer
    }


workTimeout : Int
workTimeout =
    1500


pauseTimeout : Int
pauseTimeout =
    300


longPauseTimeout : Int
longPauseTimeout =
    1200


initialModel : Model
initialModel =
    { tasks = []
    , newTask = Nothing
    , lastNewId = 0
    , selectedTaskId = 0
    , timer = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


viewHeader : Html Msg
viewHeader =
    nav [ class "header" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-10" ]
                    [ h1 [] [ text "Conductor" ]
                    ]
                , div [ class "col-2 d-flex align-items-center justify-content-end" ]
                    [ a
                        [ href "#"
                        , id "new-task-btn"
                        , onClick (UpdateTask "")
                        ]
                        [ i [ class "fa fa-plus fa-2x" ] []
                        ]
                    ]
                ]
            ]
        ]


viewNewTask : Model -> Html Msg
viewNewTask model =
    case model.newTask of
        Nothing ->
            div [] []

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
        , onClick (UpdateTimer task)
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


viewActiveTaskActions : Task -> List (Html Msg)
viewActiveTaskActions task =
    [ button
        [ class "btn stop-task-btn"
        , onClick RemoveTimer
        ]
        [ i [ class "fa fa-stop" ] []
        ]
    ]


isTimerOn : Model -> Bool
isTimerOn model =
    case model.timer of
        Just _ ->
            True

        Nothing ->
            False


isActiveTask : Model -> Task -> Bool
isActiveTask model task =
    case model.timer of
        Just timer ->
            if timer.task.id == task.id then
                True

            else
                False

        Nothing ->
            False


viewTaskActions : Model -> Task -> Html Msg
viewTaskActions model task =
    let
        buttons =
            if isTimerOn model then
                if isActiveTask model task then
                    viewActiveTaskActions task

                else
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
    let
        classNames =
            if model.selectedTaskId == task.id then
                "task d-flex active"

            else
                "task d-flex"
    in
    li [ class classNames ]
        [ div
            [ class "desc flex-fill"
            , onDoubleClick (UpdateTimer task)
            , onClick (SelectTask task)
            ]
            [ text task.description ]
        , viewTaskActions model task
        ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    ul [ class "tasks" ] (List.map (viewTask model) model.tasks)


formatMinutesOrSeconds : Int -> String
formatMinutesOrSeconds value =
    String.padLeft 2 '0' (String.fromInt value)


viewTimerProgress : Timer -> Html Msg
viewTimerProgress timer =
    let
        progress =
            toFloat (timer.initialValue - timer.timeout) / toFloat timer.initialValue
    in
    div [ class "timer-progress" ]
        [ div [ class "timer-progress-ring" ]
            [ ProgressRing.viewProgress 150 6 progress ]
        , viewTimerTimeout timer
        ]


viewTimerTimeout : Timer -> Html Msg
viewTimerTimeout timer =
    let
        minutes =
            timer.timeout // 60

        seconds =
            timer.timeout - (minutes * 60)
    in
    h1 [ class "display-2 timeout" ]
        [ text (formatMinutesOrSeconds minutes)
        , text ":"
        , text (formatMinutesOrSeconds seconds)
        ]


viewTimerClassNames : Timer -> String
viewTimerClassNames timer =
    case timer.status of
        Work _ ->
            "timer timer-work"

        Pause _ ->
            "timer timer-pause"

        LongPause ->
            "timer timer-long-pause"


viewTimer : Maybe Timer -> Html Msg
viewTimer timer =
    case timer of
        Just justTimer ->
            div [ class (viewTimerClassNames justTimer) ]
                [ viewTimerProgress justTimer
                , div [ class "task-description" ]
                    [ text justTimer.task.description ]
                ]

        Nothing ->
            div [] []


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewHeader
        , viewTimer model.timer
        , viewTaskList model
        , viewNewTask model
        ]


type Msg
    = SaveTask
    | UpdateTask String
    | RemoveTask Task
    | UpdateTimer Task
    | SelectTask Task
    | Tick Time.Posix
    | FocusResult (Result Browser.Dom.Error ())
    | OnKeyPressed String
    | RemoveTimer


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


createTimer : Task -> Timer
createTimer task =
    Timer task workTimeout workTimeout (Work 1) 1


updateExistingTimer : Timer -> Task -> Timer
updateExistingTimer timer task =
    case timer.status of
        Pause count ->
            { timer
                | status = Work (count + 1)
                , initialValue = workTimeout
                , timeout = workTimeout
                , task = task
            }

        LongPause ->
            { timer
                | status = Work 1
                , initialValue = workTimeout
                , timeout = workTimeout
                , task = task
            }

        _ ->
            timer


updateTimer : Model -> Task -> Model
updateTimer model task =
    let
        newTimer =
            case model.timer of
                Nothing ->
                    createTimer task

                Just timer ->
                    updateExistingTimer timer task
    in
    { model | timer = Just newTimer }


shiftTimer : Timer -> ( Maybe Timer, Cmd Msg )
shiftTimer timer =
    case timer.status of
        Work 4 ->
            let
                newTimer =
                    { timer
                        | status = LongPause
                        , initialValue = longPauseTimeout
                        , timeout = longPauseTimeout
                    }
            in
            ( Just newTimer, notify "Take some rest..." )

        Work count ->
            let
                newTimer =
                    { timer
                        | status = Pause count
                        , initialValue = pauseTimeout
                        , timeout = pauseTimeout
                    }
            in
            ( Just newTimer, notify "Take a breathe..." )

        _ ->
            ( Just timer, Cmd.none )


decreaseTimer : Timer -> ( Maybe Timer, Cmd Msg )
decreaseTimer timer =
    case timer.timeout of
        0 ->
            shiftTimer timer

        _ ->
            let
                newTimer =
                    { timer | timeout = timer.timeout - 1 }
            in
            ( Just newTimer, Cmd.none )


updateTimerAtTick : Model -> ( Model, Cmd Msg )
updateTimerAtTick model =
    case model.timer of
        Nothing ->
            ( model, Cmd.none )

        Just currentTimer ->
            let
                ( newTimer, cmd ) =
                    decreaseTimer currentTimer
            in
            ( { model | timer = newTimer }, cmd )


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
            if isActiveTask model selectedTask then
                model

            else
                removeTask model selectedTask


toggleTimerForSelectedTask : Model -> Maybe Task -> Cmd Msg
toggleTimerForSelectedTask model maybeSelectedTask =
    case maybeSelectedTask of
        Nothing ->
            Cmd.none

        Just selectedTask ->
            if isActiveTask model selectedTask then
                send RemoveTimer

            else
                send (UpdateTimer selectedTask)


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
            ( model, toggleTimerForSelectedTask model maybeSelectedTask )

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
                    Task 0 "" False
            in
            ( { model | newTask = Just task }
            , Task.attempt FocusResult (focus "new-task-description")
            )

        FocusResult _ ->
            ( model, Cmd.none )

        UpdateTask description ->
            let
                task =
                    Task 0 description False
            in
            ( { model | newTask = Just task }
            , Cmd.none
            )

        RemoveTask task ->
            ( removeTask model task, Cmd.none )

        UpdateTimer task ->
            ( updateTimer model task, Cmd.none )

        SelectTask task ->
            ( { model | selectedTaskId = task.id }, Cmd.none )

        Tick _ ->
            updateTimerAtTick model

        OnKeyPressed key ->
            case model.newTask of
                Nothing ->
                    processKey model key

                Just _ ->
                    ( model, Cmd.none )

        RemoveTimer ->
            ( { model | timer = Nothing }
            , Cmd.none
            )


keyDecoder : Decoder String
keyDecoder =
    field "key" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , onKeyPress <| map OnKeyPressed keyDecoder
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
