module Model exposing (Model, initial, Orientation(..), Wall(..))

import Window exposing (Size)
import ArrowKeys
import Camera


type Orientation
    = Horizontal
    | Vertical


type alias Position =
    { x : Float
    , y : Float
    }


type Wall
    = Wall Orientation Position Float


type alias Model =
    { tick : Float
    , windowSize : Size
    , arrowKeys : ArrowKeys.Model
    , position : Position
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
    , position =
        { x = 450
        , y = 75
        }
    , camera =
        { x = 450
        , y = 75
        }
    , walls =
        [ Wall Horizontal { x = 450, y = 0 } 900
        , Wall Horizontal { x = 450, y = 150 } 900
        , Wall Vertical { x = 0, y = 75 } 150
        , Wall Vertical { x = 900, y = 75 } 150
        ]
    , width = 900
    , height = 150
    }
