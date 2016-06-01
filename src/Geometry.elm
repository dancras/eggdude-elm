module Geometry exposing (..)

import Math.Vector2 as Vector2


type Point
    = Point Float Float


type alias Vector =
    Vector2.Vec2


type LineSegment
    = LineSegment Point Point


type Circle
    = Circle Point Float


pointToTuple : Point -> ( Float, Float )
pointToTuple (Point x y) =
    ( x, y )


pointToRecord : Point -> { x : Float, y : Float }
pointToRecord (Point x y) =
    { x = x, y = y }


getPointX : Point -> Float
getPointX (Point x _) =
    x


getPointY : Point -> Float
getPointY (Point _ y) =
    y


vectorBetween : Point -> Point -> Vector
vectorBetween (Point startX startY) (Point endX endY) =
    Vector2.vec2 (endX - startX) (endY - startY)


carryPoint : Vector -> Point -> Point
carryPoint vector (Point x y) =
    Point (x + Vector2.getX vector) (y + Vector2.getY vector)


pointInsideCircle : Circle -> Point -> Bool
pointInsideCircle (Circle (Point centerX centerY) radius) (Point pointX pointY) =
    (pointX - centerX) ^ 2 + (pointY - centerY) ^ 2 <= radius ^ 2


intersectsCircle : Circle -> LineSegment -> Bool
intersectsCircle circle (LineSegment start end) =
    let
        (Circle center r) =
            circle

        startToEnd =
            vectorBetween start end

        startToCircle =
            vectorBetween start center

        projectionLength =
            Vector2.dot startToCircle (Vector2.normalize startToEnd)

        projection =
            Vector2.scale projectionLength (Vector2.normalize startToEnd)

        closest =
            if (projectionLength < 0) then
                start
            else if (projectionLength > Vector2.length startToEnd) then
                end
            else
                carryPoint projection start
    in
        pointInsideCircle circle closest
