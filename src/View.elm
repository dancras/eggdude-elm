module View exposing (view)

import Collage exposing (..)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Html exposing (Html)
import Model exposing (Model)
import Update exposing (Msg)


modelThroughCamera : Model -> Model
modelThroughCamera model =
    { model
        | position =
            { x = model.position.x - model.cameraPosition.x
            , y = model.position.y
            }
    }


view : Model -> Html Msg
view model' =
    let
        model =
            modelThroughCamera model'

        windowSize =
            model.windowSize

        ( w, h ) =
            ( toFloat windowSize.width, toFloat windowSize.height )
    in
        collage windowSize.width
            windowSize.height
            [ rect w h |> filled (rgb 240 250 230)
            , circle 50
                |> filled (rgb 0 0 0)
                |> move ( model.position.x, model.position.y )
            , circle 10
                |> filled (rgb 40 40 160)
                |> move ( -model.cameraPosition.x, 0 )
            ]
            |> toHtml
