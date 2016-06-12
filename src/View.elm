module View exposing (view)

import Collage exposing (..)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Html exposing (Html)
import Model exposing (Model)
import Update exposing (Msg)
import List
import Geometry exposing (Orientation(..), pointToTuple)
import Maze exposing (..)


view : Model -> Html Msg
view model =
    let
        windowSize =
            model.windowSize

        ( w, h ) =
            ( toFloat windowSize.width, toFloat windowSize.height )
    in
        collage windowSize.width
            windowSize.height
            (List.concat
                [ [ rect w h |> filled (rgb 240 250 230) ]
                , (List.map (\form -> move ( -model.camera.x, -model.camera.y ) form)
                    <| [ circle 50
                            |> filled (rgb 0 0 0)
                            |> move (pointToTuple model.player.position)
                       , circle 10
                            |> filled (rgb 40 40 160)
                            |> move ( 100, 75 )
                       ]
                    ++ List.map (drawWall model.width model.height) model.walls
                  )
                ]
            )
            |> toHtml


drawWall : Float -> Float -> Wall -> Form
drawWall w h wall =
    case wall of
        Wall Horizontal { x, y } length ->
            rect length 30
                |> filled (rgb 0 100 0)
                |> move ( x, y )

        Wall Vertical { x, y } length ->
            rect 30 length
                |> filled (rgb 0 80 0)
                |> move ( x, y )
