module CollisionSpec exposing (..)

import ElmTestBDDStyle exposing (..)
import Math.Vector2 as Vector2
import Collision exposing (..)
import Geometry exposing (..)


circle : Circle
circle =
    Circle (Point 0 0) 10


tests : Test
tests =
    describe "Collision"
        [ describe "avoidCollision"
            [ it "returns movement when there is no collision"
                <| let
                    movement =
                        Vector2.vec2 10 0

                    lineSegment =
                        LineSegment (Point 20 -10) (Point 20 10)
                   in
                    expect (avoidCollision circle lineSegment movement) toBe movement
            , it "returns adjusted movement when there is a collision"
                <| let
                    movement =
                        Vector2.vec2 15 0

                    lineSegment =
                        LineSegment (Point 20 -10) (Point 20 10)

                    expectedMovement =
                        Vector2.vec2 10 0
                   in
                    expect (avoidCollision circle lineSegment movement) toBe expectedMovement
            ]
        ]
