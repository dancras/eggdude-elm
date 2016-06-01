module Update exposing (Msg(..), update)

import Time exposing (Time)
import Window exposing (Size)
import Model exposing (Model)
import ArrowKeys exposing (KeyState(..))
import Camera
import Geometry exposing (..)
import List


type Msg
    = ArrowKeysMsg ArrowKeys.Msg
    | Tick Time
    | WindowSize Size
    | Noop


directionalForce : ArrowKeys.Model -> { x : Float, y : Float }
directionalForce { leftKey, upKey, rightKey, downKey } =
    let
        force =
            if leftKey /= rightKey && upKey /= downKey then
                sin 45
            else
                1
    in
        { x =
            if leftKey == rightKey then
                0
            else if leftKey == KeyDown then
                -force
            else
                force
        , y =
            if upKey == downKey then
                0
            else if upKey == KeyDown then
                force
            else
                -force
        }


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


hasCollision : List Model.Wall -> Edge -> { x : Float, y : Float } -> Bool
hasCollision walls edge position =
    List.any (intersectsCircle (Circle (Point position.x position.y) 50)) (List.map (getEdge edge) walls)


avoidCollisions : Model -> { x : Float, y : Float } -> { x : Float, y : Float } -> { x : Float, y : Float }
avoidCollisions { position, walls } df newPosition =
    { newPosition
        | y =
            if
                (df.y > 0 && hasCollision walls Bottom newPosition)
                    || (df.y < 0 && hasCollision walls Top newPosition)
            then
                position.y
            else
                newPosition.y
        , x =
            if
                (df.x > 0 && hasCollision walls Left newPosition)
                    || (df.x < 0 && hasCollision walls Right newPosition)
            then
                position.x
            else
                newPosition.x
    }


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
                    directionalForce model.arrowKeys

                newPosition =
                    avoidCollisions model
                        df
                        { x = model.position.x + df.x * (pixelsPerSecond 400 delta)
                        , y = model.position.y + df.y * (pixelsPerSecond 400 delta)
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
