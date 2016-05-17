module Model exposing (Model, initial)

import Window exposing (Size)
import ArrowKeys


type alias Model =
    { tick : Float
    , inc : Int
    , windowSize : Size
    , arrowKeys : ArrowKeys.Model
    , position : ( Float, Float )
    }


initial : Model
initial =
    { tick = 0
    , inc = 0
    , windowSize =
        { width = 0
        , height = 0
        }
    , arrowKeys = ArrowKeys.initial
    , position = ( 0, 0 )
    }
