module Timer exposing (Model, Msg, init, setActive, shift, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import ProgressRing
import Time


type Statuts
    = Work Int
    | Pause Int
    | LongPause
    | Ready Int


type alias Model =
    { label : String
    , initialValue : Int
    , timeout : Int
    , status : Statuts
    , isActive : Bool
    }


init : Model
init =
    Model "Ready" 0 0 (Ready 1) False



-- Utility methods


setActive : Model -> Bool -> Model
setActive model isActive =
    { model | isActive = isActive }


shift : Model -> String -> Model
shift model label =
    case model.status of
        Ready count ->
            { model
                | status = Work count
                , initialValue = 1500
                , timeout = 0
                , label = label
                , isActive = True
            }

        Work 4 ->
            { model
                | status = LongPause
                , initialValue = 1200
                , timeout = 0
                , isActive = True
            }

        Work count ->
            { model
                | status = Pause count
                , initialValue = 300
                , timeout = 0
                , isActive = True
            }

        Pause count ->
            { model
                | status = Ready (count + 1)
                , initialValue = 0
                , timeout = 0
                , isActive = False
            }

        LongPause ->
            { model
                | status = Ready 1
                , initialValue = 0
                , timeout = 0
                , isActive = False
            }



-- VIEW


formatMinutesOrSeconds : Int -> String
formatMinutesOrSeconds value =
    String.padLeft 2 '0' (String.fromInt value)


viewTimerTimeout : Model -> Html Msg
viewTimerTimeout model =
    let
        remainingTime =
            model.initialValue - model.timeout

        minutes =
            remainingTime // 60

        seconds =
            remainingTime - (minutes * 60)
    in
    h1 [ class "timeout" ]
        [ text (formatMinutesOrSeconds minutes)
        , text ":"
        , text (formatMinutesOrSeconds seconds)
        ]


viewTimerProgress : Model -> Html Msg
viewTimerProgress model =
    let
        progress =
            if model.isActive then
                toFloat model.timeout / toFloat model.initialValue

            else
                0.0
    in
    div [ class "timer-progress" ]
        [ div [ class "timer-progress-ring" ]
            [ ProgressRing.viewProgress 150 6 progress ]
        , viewTimerTimeout model
        ]


viewTimerClassNames : Model -> String
viewTimerClassNames model =
    if model.isActive then
        case model.status of
            Work _ ->
                "timer timer-work"

            Pause _ ->
                "timer timer-pause"

            LongPause ->
                "timer timer-long-pause"

            Ready _ ->
                "timer timer-inactive"

    else
        "timer timer-inactive"


viewTimerLabel : Model -> Html Msg
viewTimerLabel model =
    if model.isActive then
        case model.status of
            Work _ ->
                text model.label

            Pause _ ->
                text "Take a breathe..."

            LongPause ->
                text "Take some rest..."

            Ready _ ->
                text "Ready to go"

    else
        text "Ready to go"


view : Model -> Html Msg
view model =
    div [ class (viewTimerClassNames model) ]
        [ viewTimerProgress model
        , div [ class "task-description" ]
            [ viewTimerLabel model ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isActive then
        Time.every 1000 Tick

    else
        Sub.none



-- update


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.timeout == model.initialValue then
                let
                    newModel =
                        shift model model.label
                in
                ( newModel, Cmd.none )

            else
                ( { model | timeout = model.timeout + 1 }
                , Cmd.none
                )
