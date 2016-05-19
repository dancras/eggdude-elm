module View exposing (view)

import Collage exposing (..)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Html exposing (Html)
import Model exposing (Model)
import Update exposing (Msg)
import List


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
                , (List.map (\form -> move ( -model.camera.x, 0 ) form)
                    [ circle 50
                        |> filled (rgb 0 0 0)
                        |> move ( model.position.x, model.position.y )
                    , circle 10
                        |> filled (rgb 40 40 160)
                        |> move ( 0, 0 )
                    ]
                  )
                ]
            )
            |> toHtml
