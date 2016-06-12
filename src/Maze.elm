module Maze exposing (..)

import Geometry exposing (..)


type alias Position =
    { x : Float
    , y : Float
    }


type Wall
    = Wall Orientation Position Float


getEdge : Edge -> Wall -> LineSegment
getEdge edge (Wall orientation { x, y } length) =
    let
        ( shiftX, shiftY ) =
            if orientation == Horizontal then
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


getWallEdges : List Wall -> Edge -> List LineSegment
getWallEdges walls edge =
    List.map (getEdge edge) walls
