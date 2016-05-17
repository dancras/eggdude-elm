module ArrowKeys exposing (KeyState(..), Model, Msg, initial, subscriptions, update)

import Keyboard exposing (KeyCode)


type KeyState
    = KeyDown
    | KeyUp


type alias Model =
    { leftKey : KeyState
    , upKey : KeyState
    , rightKey : KeyState
    , downKey : KeyState
    }


type Msg
    = LeftKeyChange KeyState
    | UpKeyChange KeyState
    | RightKeyChange KeyState
    | DownKeyChange KeyState
    | Noop


initial : Model
initial =
    { leftKey = KeyUp
    , upKey = KeyUp
    , rightKey = KeyUp
    , downKey = KeyUp
    }


subscriptions : Model -> List (Sub Msg)
subscriptions model =
    [ Keyboard.ups (onKeyChange KeyUp)
    , Keyboard.downs (onKeyChange KeyDown)
    ]


update : Msg -> Model -> Model
update msg model =
    case (Debug.log "update" msg) of
        LeftKeyChange keyState ->
            { model
                | leftKey = keyState
            }

        UpKeyChange keyState ->
            { model
                | upKey = keyState
            }

        RightKeyChange keyState ->
            { model
                | rightKey = keyState
            }

        DownKeyChange keyState ->
            { model
                | downKey = keyState
            }

        _ ->
            model


onKeyChange : KeyState -> KeyCode -> Msg
onKeyChange state keyCode =
    case keyCode of
        37 ->
            LeftKeyChange (Debug.log "lstate" state)

        38 ->
            UpKeyChange state

        39 ->
            RightKeyChange (Debug.log "rstate" state)

        40 ->
            DownKeyChange state

        _ ->
            Noop
