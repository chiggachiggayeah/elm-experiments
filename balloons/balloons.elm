import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

-- Model
type alias Loon =
  { x: Float
  , y: Float
  , b: Form
  }

balloon : (Float, Float) -> Form
balloon (x, y) =
  group
    [ circle 75
        |> filled charcoal
        |> move(x, y)
      ,ngon 3 81
        |> filled charcoal
        |> move (x, -68 + y)
        |> rotate (degrees -90)
    ]


bShape = balloon (-20, -100)

aBalloon : Loon
aBalloon =
  { x =  -20
  , y =  -100
  , b =  bShape
  }

view : Loon -> Element
view loon = collage 1000 1000 [ loon.b ]

update : Float -> Loon -> Loon
update dt loon =
  if (loon.y + (dt/0.9)) < 500 then
    { loon |
      b = loon.b |> moveY (dt/0.9),
      y = loon.y + (dt/0.9)
    }
  else
    loon

iter : Signal Float
iter = Signal.map (\t -> t/20) (fps 30)

main : Signal Element
main = Signal.map view (Signal.foldp update aBalloon iter)




-- Update
-- View
