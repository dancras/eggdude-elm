module Model exposing (Model, initial)

import Window exposing (Size)
import ArrowKeys
import Camera


type alias Model =
    { tick : Float
    , windowSize : Size
    , arrowKeys : ArrowKeys.Model
    , position :
        { x : Float
        , y : Float
        }
    , camera : Camera.Model
    }


initial : Model
initial =
    { tick = 0
    , windowSize =
        { width = 0
        , height = 0
        }
    , arrowKeys = ArrowKeys.initial
    , position =
        { x = 0
        , y = 0
        }
    , camera = Camera.initial
    }
