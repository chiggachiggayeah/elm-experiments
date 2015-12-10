{-
  In trying to learn elm, I saw a few things utilizing physics.

  With the nature of signals, something requiring a system of regular updateLoons
  seemed interesting so I thought I would give it a go.

  Goals:
    [x] Vertical movement
    [x] Random Gen (Color, Pos)
    [] Collisions and Horizontal movement
    [] String
    [] Mouse interaction, popping
-}

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (foldr, map)
import Text exposing (..)
import Random exposing (..)
import Window
import Time exposing (..)

-- Model
type alias Loon =
  { x: Float
  , y: Float
  , vx: Float
  , vy: Float
  , b: Form
  }

type alias Sim =
  { allLoons : List Loon
  , deltaSum : Float
  }

baseHeight = 500
baseWidth = 500

radius : Float
radius = baseWidth / 10

-- type Params =

getHeight : Int -> Float
getHeight h = toFloat h

runGen : Seed -> (Int, Seed)
runGen seed =
  let
    gen = int -255 255
  in
    generate gen seed

genInitSeed : Seed
genInitSeed = initialSeed 36758

getSeed : (Int, Seed) -> Seed
getSeed (i, s) = s

getInt : (Int, Seed) -> Int
getInt (i, s) = i

balloon : (Float, Float) -> Form
balloon (x, y) =
  let
    newColor = (rgba
                (floor (abs x))
                (floor (abs (y + baseHeight)))
                (min 75 (floor (abs (x + y + baseHeight))))
                1.0)
    ribbon = traced (solid red) (path [(x, y - (radius * 7)), (x, y - radius * 2)])
  in
    group
      [ circle radius
          |> filled newColor
          |> move(x, y)
        ,ngon 3 radius
          |> filled newColor
          |> move (x, -50 + y)
          |> rotate (degrees -90)
        ,ribbon
      ]

bShape = balloon (-20, 0)

aBalloon : Loon
aBalloon =
  { x =  -20
  , y =  0
  , b =  bShape
  , vx = 0
  , vy = 0
  }

bBalloon : Loon
bBalloon =
  { x = 70
  , y = 10
  , b = balloon (80, 10)
  , vx = 0
  , vy = 0
  }

simulation : Sim
simulation =
  { allLoons = [ aBalloon, bBalloon ]
  , deltaSum = 0
  }

-- control how the shapes are affected visually
toTuple {x, y} = (x, y)


view : (Int, Int) -> Sim -> Element
view (w', h') sim =
  let
    gameScale = min (toFloat h' / baseHeight) 1
    allLoons = (List.map .b sim.allLoons)
  in
    collage w' h' (List.map (scale gameScale) allLoons)


-- compute the physics on the model
updateLoon : Float -> Loon -> Loon
updateLoon dt loon =
  let
    delta = (dt / 100)
    -- h = Signal.map getHeight  Window.dimensions
  in
    { loon |
        x = loon.x,
        y = if loon.y < (baseHeight - (radius * 3))
              then
                loon.y + loon.vy
              else
                loon.y,
        vx = loon.vx,
        vy = if loon.vy < 20 then loon.vy + delta else loon.vy,
        b = if (loon.y) < (baseHeight - (radius * 3))
              then
                moveY (loon.vy) loon.b
              else
                loon.b
    }

updateLoons : Float -> Sim -> Sim
updateLoons dt sim =
  let
    -- append to loons
    -- random color
    -- loons = sim.allLoons
    balloons =
      let
        thisXSeed = initialSeed (floor sim.deltaSum)
        thisYSeed = initialSeed (ceiling (sim.deltaSum * 5))
        randXVal = getInt ( runGen thisXSeed )
        randYVal = getInt ( runGen thisYSeed )
        newLoon : Loon
        newLoon =
          { x = toFloat (randXVal)
          , y = toFloat (randYVal - baseHeight)
          , vx = 0
          , vy = 0
          , b = balloon(toFloat(randXVal), toFloat(randYVal - baseHeight))
          }
      in
        -- newLoon :: loons
        if (sim.deltaSum / toFloat (List.length sim.allLoons)) > 30  then newLoon :: sim.allLoons else sim.allLoons
  in
    { sim |
        allLoons = (List.map (updateLoon dt) balloons),
        deltaSum = sim.deltaSum + dt
    }


showLoons sim = show sim


iter : Signal Float
iter = Signal.map (\t -> t/50) (fps 60)

toPair : Loon -> Float -> (Loon, Float)
toPair loon fl = (loon, fl)

simStep : Signal Sim
simStep =  (Signal.foldp updateLoons simulation iter)

runSim : Signal Element
runSim = Signal.map2 view Window.dimensions simStep

showSim : Signal Element
showSim = Signal.map showLoons simStep

main : Signal Element
main = runSim
