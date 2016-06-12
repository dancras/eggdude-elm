module Update exposing (Msg(..), update)

import Time exposing (Time)
import Window exposing (Size)
import Model exposing (Model)
import ArrowKeys exposing (KeyState(..))
import Camera
import Collision exposing (..)
import Geometry exposing (..)
import Math.Vector2 as Vector2
import List
import Maze exposing (..)


type Msg
    = ArrowKeysMsg ArrowKeys.Msg
    | Tick Time
    | WindowSize Size
    | Noop


directionalForce : ArrowKeys.Model -> Vector2.Vec2
directionalForce { leftKey, upKey, rightKey, downKey } =
    let
        force =
            if leftKey /= rightKey && upKey /= downKey then
                sin 45
            else
                1

        fx =
            if leftKey == rightKey then
                0
            else if leftKey == KeyDown then
                -force
            else
                force

        fy =
            if upKey == downKey then
                0
            else if upKey == KeyDown then
                force
            else
                -force
    in
        Vector2.vec2 fx fy


pixelsPerSecond : Float -> Time -> Float
pixelsPerSecond pps delta =
    delta / (1000 / pps)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrowKeysMsg msg ->
            ( { model
                | arrowKeys = ArrowKeys.update msg model.arrowKeys
              }
            , Cmd.none
            )

        Tick delta ->
            let
                proposedMovement =
                    Vector2.scale (pixelsPerSecond 400 delta) (directionalForce model.arrowKeys)

                movement =
                    List.foldr (avoidCollision (Circle model.player.position 50))
                        proposedMovement
                        (List.concatMap (getWallEdges model.walls) (getCollisionEdges proposedMovement))

                newPosition =
                    carryPoint movement model.player.position
            in
                ( { model
                    | tick = model.tick + delta
                    , player =
                        { position = newPosition
                        }
                    , camera = Camera.follow 200 model.camera (pointToRecord newPosition)
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
