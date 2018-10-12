module Timer exposing (Model, shift, start, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import ProgressRing


type Statuts
    = Work Int
    | Pause Int
    | LongPause
    | Inactive


type alias Model =
    { label : String
    , initialValue : Int
    , timeout : Int
    , status : Statuts
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



-- Start


start : Model -> String -> Model
start model label =
    case model.status of
        Pause count ->
            { model
                | status = Work (count + 1)
                , initialValue = workTimeout
                , timeout = workTimeout
                , label = label
            }

        LongPause ->
            { model
                | status = Work 1
                , initialValue = workTimeout
                , timeout = workTimeout
                , label = label
            }

        Inactive ->
            { model
                | status = Work 1
                , initialValue = workTimeout
                , timeout = workTimeout
                , label = label
            }

        _ ->
            model



-- shift


shift : Model -> Model
shift model =
    case model.status of
        Work 4 ->
            { model
                | status = LongPause
                , initialValue = longPauseTimeout
                , timeout = longPauseTimeout
            }

        Work count ->
            { model
                | status = Pause count
                , initialValue = pauseTimeout
                , timeout = pauseTimeout
            }

        Pause _ ->
            { model | status = Inactive }

        LongPause ->
            { model | status = Inactive }

        _ ->
            model



-- VIEW


formatMinutesOrSeconds : Int -> String
formatMinutesOrSeconds value =
    String.padLeft 2 '0' (String.fromInt value)


viewTimerTimeout : Model -> Html msg
viewTimerTimeout model =
    let
        minutes =
            model.timeout // 60

        seconds =
            model.timeout - (minutes * 60)
    in
    h1 [ class "timeout" ]
        [ text (formatMinutesOrSeconds minutes)
        , text ":"
        , text (formatMinutesOrSeconds seconds)
        ]


viewTimerProgress : Model -> Html msg
viewTimerProgress model =
    let
        progress =
            case model.status of
                Inactive ->
                    0.0

                _ ->
                    toFloat (model.initialValue - model.timeout) / toFloat model.initialValue
    in
    div [ class "timer-progress" ]
        [ div [ class "timer-progress-ring" ]
            [ ProgressRing.viewProgress 150 6 progress ]
        , viewTimerTimeout model
        ]


viewTimerClassNames : Model -> String
viewTimerClassNames model =
    case model.status of
        Work _ ->
            "timer timer-work"

        Pause _ ->
            "timer timer-pause"

        LongPause ->
            "timer timer-long-pause"

        Inactive ->
            "timer timer-inactive"


view : Model -> Html msg
view model =
    div [ class (viewTimerClassNames model) ]
        [ viewTimerProgress model
        , div [ class "task-description" ]
            [ text model.label ]
        ]
