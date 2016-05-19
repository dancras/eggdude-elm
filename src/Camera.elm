module Camera exposing (..)


type alias Model =
    { x : Float
    , y : Float
    }


initial : Model
initial =
    { x = 0
    , y = 0
    }


follow : Float -> Model -> Model -> Model
follow bound camera subject =
    { x =
        if subject.x - camera.x > bound then
            subject.x - bound
        else if subject.x - camera.x < -bound then
            subject.x + bound
        else
            camera.x
    , y = camera.y
    }
