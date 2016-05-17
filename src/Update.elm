module Update exposing (Msg(..), update)

import Time exposing (Time)
import Window exposing (Size)
import Model exposing (Model)
import ArrowKeys exposing (KeyState(..))


type Msg
    = ArrowKeysMsg ArrowKeys.Msg
    | Increment
    | Decrement
    | Tick Time
    | WindowSize Size
    | Noop


directionalForce : ArrowKeys.Model -> ( Float, Float )
directionalForce { leftKey, upKey, rightKey, downKey } =
    let
        force =
            if leftKey /= rightKey && upKey /= downKey then
                sin 45
            else
                1
    in
        ( if leftKey == rightKey then
            0
          else if leftKey == KeyDown then
            -force
          else
            force
        , if upKey == downKey then
            0
          else if upKey == KeyDown then
            force
          else
            -force
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrowKeysMsg msg ->
            ( { model
                | arrowKeys = ArrowKeys.update msg model.arrowKeys
              }
            , Cmd.none
            )

        Increment ->
            ( { model
                | inc = model.inc + 1
              }
            , Cmd.none
            )

        Decrement ->
            ( { model
                | inc = model.inc - 1
              }
            , Cmd.none
            )

        Tick delta ->
            let
                df =
                    directionalForce model.arrowKeys
            in
                ( { model
                    | tick = model.tick + delta
                    , position = ( fst model.position + fst df * delta / 5, snd model.position + snd df * delta / 5 )
                  }
                , Cmd.none
                )

        WindowSize size ->
            ( { model
                | windowSize = size
              }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )
