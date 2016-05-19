module View exposing (view)

import Collage exposing (..)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Html exposing (Html)
import Model exposing (Model)
import Update exposing (Msg)
import ArrowKeys exposing (KeyState(..))


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
            [ rect w h |> filled (rgb 240 250 230)
            , circle 50
                |> filled (ballColor model)
                |> move (model.position)
            ]
            |> toHtml


ballColor : Model -> Color
ballColor model =
    if model.arrowKeys.leftKey == KeyDown then
        rgb 255 0 0
    else
        rgb 0 0 0
