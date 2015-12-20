module PieChart where

import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg
import Color exposing (..)

import NumExtra exposing (divRound)

pieChart : List Int -> List Color -> Clg.Form
pieChart amounts clrs = 
  let total   = max (List.sum amounts) minCircleSegments
      amounts' = List.map ((*) (total `divRound` (List.sum amounts))) amounts 
      lilAng  = 1 / toFloat total |> turns 
      accAmts = List.map List.sum (inits amounts')
      wedgeForm accAmt amt clr =
        wedge (lilAng * toFloat accAmt) lilAng amt
          |> Clg.filled clr
  in List.map3 wedgeForm accAmts amounts' clrs
       |> Clg.group 
-- TODO amounts' = reduce amounts to greatest common denom
-- TODO total    = total = greatest common factor of amounts' (also >= minCircleSegments)
-- TODO clean up

wedge : Float -> Float -> Int -> Clg.Shape
wedge aStart aOff amt = 
  let angs = List.map (\i -> aStart + toFloat i * aOff) [ 0 .. amt ]
  in Clg.polygon 
       <| (0,0) :: (List.map (curry fromPolar radius) angs)


radius : Float
radius = 1

minCircleSegments : Int
minCircleSegments = 60

-- the rotation direction around a circle
type SpinDirection 
  = Clockwise         -- e.g. from 12 o'clock to 1 o'clock
  | Counterclockwise  -- e.g. from 12 o'clock to 11 o'clock (this is the Elm default)

-- the cardinal direction in which angle 0 points
type SpinOrientation
  = TwelveOClock  -- up
  | ThreeOClock   -- to the right. (this is the Elm default)
  | SixOClock     -- down
  | NineOClock    -- to the left

type alias SpinConfig = (SpinOrientation, SpinDirection)

elmSpin : SpinConfig
elmSpin = (ThreeOClock, Counterclockwise)

pieSpin : SpinConfig
pieSpin = (TwelveOClock, Clockwise)

type Angle = Angle Float SpinConfig

-- TODO implement conversions between different SpinConfigs


-- CONVENIENCE FUNCTIONS

pctPieChart : (Int, Int) -> (Color, Color) -> Clg.Form
pctPieChart (numer, denom) (nClr, dClr) =
  pieChart [ numer, denom - numer ]
           [ nClr, dClr ]

-- from circuithub/elm-list-extra
-- TODO install that package
{-| Return all initial segments of a list, from shortest to longest, empty list first, the list itself last.
    inits [1,2,3] == [[],[1],[1,2],[1,2,3]]
-}
inits : List a -> List (List a)
inits = List.foldr (\e acc -> []::List.map ((::)e) acc) [[]]
