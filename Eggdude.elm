import Color (..)
import Debug
import Graphics.Collage (..)
import Graphics.Element (..)
import List (..)
import Keyboard
import Signal
import Time
import Window

type alias Positioned p = { p | x : Float, y : Float }

type alias Wall = Positioned(
    { w : Float
    , h : Float
    , color : Color
    })

type alias Player = Positioned({})

type alias World =
    { walls : List Wall
    , player : Player
    }

type alias Camera = Positioned({ boundX : Float })

type alias Keys = { x : Int, y : Int }

world : World
world =
    { walls =
        [ { x = -500, y = 100, w = 800, h = 30, color = rgb 200 40 40 }
        , { x = 600, y = 200, w = 600, h = 30, color = rgb 40 200 40 }
        ]
    , player = { x = 0, y = 0 }
    }

camera : Camera
camera = { x = 0, y = 0, boundX = 200 }

updateWorld : (Float, Keys) -> World -> World
updateWorld input world =
    { world |
        player <- movePlayer input world.player
    }

moveCamera : (Float, Keys) -> Camera -> Camera
moveCamera (delta, keys) camera =
    { camera |
        x <- camera.x - delta * ((toFloat keys.x) * 5)
        {- y <- camera.y - delta * ((toFloat keys.y) * 5) -}
    }

movePlayer : (Float, Keys) -> Player -> Player
movePlayer (delta, keys) player =
    { player |
        x <- player.x + delta * ((toFloat keys.x) * 5),
        y <- player.y + delta * ((toFloat keys.y) * 5)
    }

movePositioned : (Float, Float) -> Positioned(p) -> Positioned(p)
movePositioned (moveX, moveY) p =
    { p |
        x <- p.x + moveX,
        y <- p.y + moveY
    }

worldThroughCamera : Camera -> World -> World
worldThroughCamera camera world =
    { world |
        walls <- map (movePositioned (camera.x, camera.y)) world.walls,
        player <- movePositioned (camera.x, camera.y) world.player
    }

updateCamera : World -> Camera -> Camera
updateCamera world camera =
    let newWorld = worldThroughCamera camera world
    in
        { camera |
            x <- if | newWorld.player.x > camera.boundX -> camera.x + camera.boundX - newWorld.player.x
                    | newWorld.player.x < -camera.boundX -> camera.x - camera.boundX - newWorld.player.x
                    | otherwise -> camera.x
        }

drawWall : Wall -> Form
drawWall wall =
    rect wall.w wall.h
        |> filled wall.color
        |> move (wall.x, wall.y)

view : (Int, Int) -> World -> Element
view (w', h') {walls, player} =
    let (w, h) = (toFloat w', toFloat h')
    in
        collage w' h' <| append
            [ rect w h
                |> filled (rgb 240 250 230)
            , circle 50
                |> filled (rgb 0 0 0)
                |> move (player.x, player.y)
            ]
            (map drawWall walls)

input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/15) (Time.fps 60)
      deltaArrows =
          Signal.map2 (,) delta Keyboard.arrows
  in
      Signal.sampleOn delta deltaArrows

main : Signal Element
main =
    let worldStream = Signal.foldp updateWorld world input
        cameraStream = Signal.foldp updateCamera camera worldStream
    in
        Signal.map2 view Window.dimensions <|
            Signal.map2 worldThroughCamera cameraStream worldStream
