module Main exposing (..)

import AnimationFrame
import Html.App
import Task
import Window
import Model exposing (Model, initial)
import Update exposing (Msg, update)
import View exposing (view)
import ArrowKeys
import List exposing (append)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        <| append
            [ AnimationFrame.diffs Update.Tick
            , Window.resizes Update.WindowSize
            ]
            (List.map (Sub.map Update.ArrowKeysMsg) (ArrowKeys.subscriptions model.arrowKeys))


init : ( Model, Cmd Msg )
init =
    ( initial
    , Task.perform (always Update.Noop) (Update.WindowSize) Window.size
    )



{- The type of main in elm is Program flags -}


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
