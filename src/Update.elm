module Update exposing (Msg(..), update)

import Time exposing (Time)
import Window exposing (Size)
import Model exposing (Model)
import ArrowKeys exposing (KeyState(..))
import Camera


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

                newPosition =
                    { x = model.position.x + fst df * delta / 2.5
                    , y = model.position.y + snd df * delta / 2.5
                    }
            in
                ( { model
                    | tick = model.tick + delta
                    , position = newPosition
                    , camera = Camera.follow 200 model.camera newPosition
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
