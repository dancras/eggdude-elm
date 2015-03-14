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

type alias Movable m = Positioned({ m | vx: Float, vy: Float, ax: Float, ay: Float })

type alias Wall = Positioned(
    { w : Float
    , h : Float
    , color : Color
    })

type alias Player = Movable(
    { facingX : Int,
      facingY : Int
    })

type alias World =
    { walls : List Wall
    , player : Player
    , followers : List( Movable({}) )
    }

type alias Camera = Positioned({ boundX : Float })

type alias Keys = { x : Int, y : Int }

world : World
world =
    { walls =
        [ { x = -500, y = 100, w = 800, h = 30, color = rgb 200 40 40 }
        , { x = 600, y = 200, w = 600, h = 30, color = rgb 40 200 40 }
        ]
    , player = { x = 0, y = 0, vx = 0, vy = 0, ax = 0, ay = 0, facingX = 1, facingY = 0 }
    , followers = [ { x = 100, y = 0, vx = 0, vy = 0, ax = 0, ay = 0 }
                  , { x = 200, y = 0, vx = 0, vy = 0, ax = 0, ay = 0 }
                  , { x = 300, y = 0, vx = 0, vy = 0, ax = 0, ay = 0 }
                  , { x = 400, y = 0, vx = 0, vy = 0, ax = 0, ay = 0 }
                  ]
    }

camera : Camera
camera = { x = 0, y = 0, boundX = 200 }

updateWorld : (Float, Keys) -> World -> World
updateWorld (delta, keys) world =
    { world |
        player <- world.player
            |> updateAcceleration keys
            |> updatePlayerFacing keys
            |> resistanceDueToVelocity 0.02
            |> updateVelocity
            |> updatePosition delta
            |> Debug.watch "player",
        followers <- world.followers
            |> map2 updateAcceleration (followerKeys (toPositioned world.player) (map toPositioned world.followers))
            |> map (resistanceDueToVelocity 0.015)
            |> map updateVelocity
            |> map (updatePosition delta)
            |> Debug.watch "followers"
    }

toPositioned : Positioned(a) -> Positioned({})
toPositioned p = { x = p.x, y = p.y }

followerKeys : Positioned a -> List(Positioned a) -> List(Keys)
followerKeys firstTarget followers =
    map2 followerAI (firstTarget :: followers) followers

followerAI : Positioned(a) -> Positioned(b) -> Keys
followerAI target follower =
    let distance = sqrt ((target.x - follower.x)^2 + (target.y - follower.y)^2)
    in
        if | distance > 100 -> { x = if | target.x - follower.x > 5 -> 1
                                        | target.x - follower.x < -5 -> -1
                                        | otherwise -> 0
                               , y = if | target.y - follower.y > 5 -> 1
                                        | target.y - follower.y < -5 -> -1
                                        | otherwise -> 0
                               }
           | otherwise -> { x = 0, y = 0 }

firstFollowerTarget : Player -> (Float, Float)
firstFollowerTarget player =
    ( if | player.facingX > 0 -> player.x - 80
         | player.facingX < 0 -> player.x + 80
         | otherwise -> player.x
    , if | player.facingY > 0 -> player.y - 80
         | player.facingY < 0 -> player.y + 80
         | otherwise -> player.y
    )

updateFollowerAcceleration : Player -> (Float, Float) -> Movable(m) -> Movable(m)
updateFollowerAcceleration player (x, y) follower =
    let a = sqrt ((player.x - follower.x)^2 + (player.y - follower.y)^2)
    in
        { follower |
            ax <- (x - follower.x) / (200 - a),
            ay <- (y - follower.y) / (200 - a)
        }

updateAcceleration : Keys -> Movable(m) -> Movable(m)
updateAcceleration keys subject =
    let a = if | keys.x /= 0 && keys.y /= 0 -> 1
               | otherwise -> 2
    in
        { subject |
            ax <- if | keys.x > 0 -> a
                     | keys.x < 0 -> -a
                     | otherwise -> max -a <| min a <| -subject.vx,
            ay <- if | keys.y > 0 -> a
                     | keys.y < 0 -> -a
                     | otherwise -> max -a <| min a <| -subject.vy
        }

resistanceDueToVelocity : Float -> Movable(m) -> Movable(m)
resistanceDueToVelocity coefficient subject =
    { subject |
        ax <- subject.ax - (abs subject.vx) * subject.vx * coefficient,
        ay <- subject.ay - (abs subject.vy) * subject.vy * coefficient
    }

updateVelocity : Movable(m) -> Movable(m)
updateVelocity subject =
    { subject |
        vx <- subject.vx + subject.ax,
        vy <- subject.vy + subject.ay
    }

updatePosition : Float -> Movable(m) -> Movable(m)
updatePosition delta subject =
    { subject |
        x <- subject.x + delta * subject.vx,
        y <- subject.y + delta * subject.vy
    }

updatePlayerFacing : Keys -> Player -> Player
updatePlayerFacing keys player =
    { player |
        facingX <- if | keys.x == 0 && keys.y == 0 -> player.facingX
                      | otherwise -> keys.x,
        facingY <- if | keys.x == 0 && keys.y == 0 -> player.facingY
                      | otherwise -> keys.y
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
        player <- movePositioned (camera.x, camera.y) world.player,
        followers <- map (movePositioned (camera.x, camera.y)) world.followers
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

drawFollower : Movable(m) -> Form
drawFollower follower =
    circle 20
        |> filled (rgb 0 0 100)
        |> move (follower.x, follower.y)

view : (Int, Int) -> World -> Element
view (w', h') {walls, player, followers} =
    let (w, h) = (toFloat w', toFloat h')
    in
        collage w' h' <| concat
            [
                [ rect w h
                    |> filled (rgb 240 250 230)
                ]
            ,   (map drawFollower followers)
            ,   [ circle 50
                    |> filled (rgb 0 0 0)
                    |> move (player.x, player.y)
                ]
            ,   (map drawWall walls)
            ]

input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/30) (Time.fps 60)
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
