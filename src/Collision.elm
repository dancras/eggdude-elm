module Collision exposing (..)

import Math.Vector2 as Vector2
import Geometry exposing (..)


avoidCollision : Circle -> LineSegment -> Vector -> Vector
avoidCollision (Circle center radius) lineSegment movement =
    let
        vectorToClosestPoint =
            intersectVector (carryPoint movement center) lineSegment
    in
        if Vector2.length vectorToClosestPoint < radius then
            Vector2.add movement
                <| Vector2.scale (radius - Vector2.length vectorToClosestPoint) (Vector2.normalize vectorToClosestPoint)
        else
            movement


getCollisionEdges : Vector2.Vec2 -> List Edge
getCollisionEdges movement =
    let
        moveX =
            Vector2.getX movement

        moveY =
            Vector2.getY movement
    in
        List.concat
            [ if moveX > 0 then
                [ Left ]
              else if moveX < 0 then
                [ Right ]
              else
                []
            , if moveY > 0 then
                [ Bottom ]
              else if moveY < 0 then
                [ Top ]
              else
                []
            ]
