module Model exposing (Model, initial)

import Window exposing (Size)
import ArrowKeys
import Camera
import Geometry exposing (..)
import Maze exposing (Wall(..))


type alias Model =
    { tick : Float
    , windowSize : Size
    , arrowKeys : ArrowKeys.Model
    , player :
        { position : Point
        }
    , camera : Camera.Model
    , walls : List Wall
    , width : Float
    , height : Float
    }


initial : Model
initial =
    { tick = 0
    , windowSize =
        { width = 0
        , height = 0
        }
    , arrowKeys = ArrowKeys.initial
    , player =
        { position = Point 450 75
        }
    , camera =
        { x = 450
        , y = 75
        }
    , walls =
        [ Wall Horizontal { x = 675, y = 0 } (450 + 30)
        , Wall Horizontal { x = 450, y = 150 } (900 + 30)
        , Wall Vertical { x = 0, y = 75 } (150 + 30)
        , Wall Vertical { x = 900, y = 75 } (150 + 30)
        ]
    , width = 900
    , height = 150
    }
