module Update exposing (Msg(..), update)

import Time exposing (Time)
import Window exposing (Size)
import Model exposing (Model)
import ArrowKeys exposing (KeyState(..))
import Camera
import Geometry exposing (..)
import Math.Vector2 as Vector2
import List


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


type Edge
    = Top
    | Right
    | Bottom
    | Left


getEdge : Edge -> Model.Wall -> LineSegment
getEdge edge (Model.Wall orientation { x, y } length) =
    let
        ( shiftX, shiftY ) =
            if orientation == Model.Horizontal then
                ( length / 2, 15 )
            else
                ( 15, length / 2 )
    in
        if edge == Top then
            LineSegment (Point (x - shiftX) (y + shiftY)) (Point (x + shiftX) (y + shiftY))
        else if edge == Right then
            LineSegment (Point (x + shiftX) (y + shiftY)) (Point (x + shiftX) (y - shiftY))
        else if edge == Bottom then
            LineSegment (Point (x - shiftX) (y - shiftY)) (Point (x + shiftX) (y - shiftY))
        else
            LineSegment (Point (x - shiftX) (y + shiftY)) (Point (x - shiftX) (y - shiftY))


hasCollision : List Model.Wall -> Edge -> Point -> Bool
hasCollision walls edge position =
    List.any (intersectsCircle (Circle position 50)) (List.map (getEdge edge) walls)


avoidCollisions : Model -> Vector2.Vec2 -> Point
avoidCollisions { player, walls } movement =
    let
        proposedPosition =
            carryPoint movement player.position

        (Point proposedX proposedY) =
            proposedPosition

        moveX =
            Vector2.getX movement

        moveY =
            Vector2.getY movement

        newY =
            if
                (moveY > 0 && hasCollision walls Bottom proposedPosition)
                    || (moveY < 0 && hasCollision walls Top proposedPosition)
            then
                getPointY player.position
            else
                proposedY

        newX =
            if
                (moveX > 0 && hasCollision walls Left proposedPosition)
                    || (moveX < 0 && hasCollision walls Right proposedPosition)
            then
                getPointX player.position
            else
                proposedX
    in
        Point newX newY


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
                df =
                    Vector2.scale (pixelsPerSecond 400 delta) (directionalForce model.arrowKeys)

                newPosition =
                    avoidCollisions model df
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
