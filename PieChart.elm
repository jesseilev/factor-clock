module PieChart where

import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg
import Color exposing (..)

import NumExtra exposing (divRound)


-- MODEL 

type alias Model =
  { amounts : List Int 
  , colors : List Color -- TODO should be infinite cycle
  }


init : List Int -> List Color -> Model
init amts clrs = 
  { amounts = amts
  , colors = clrs
  } -- TODO point-free impl?


-- UPDATE

type Action
  = SetAmounts (List Int)
  | SetColors (List Color)
-- TODO mouse events


update : Action -> Model -> Model
update action model =
  case action of
    SetAmounts amts -> 
      { model |
          amounts = amts 
      }
    SetColors clrs ->
      { model | 
          colors = clrs
      }


-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model = 
  let minTotal = max (List.sum model.amounts) minCircleSegments
      total = greatestCommonFactor minTotal model.amounts
      amounts' = 
        List.map 
          ((*) (total `divRound` (List.sum model.amounts))) 
          model.amounts 
      lilAng = 1 / toFloat total |> turns 
      accAmts = List.map List.sum (inits amounts')
      wedgeForm accAmt amt clr =
        wedge (lilAng * toFloat accAmt) lilAng amt
          |> Clg.filled clr
  in 
    List.map3 wedgeForm accAmts amounts' model.colors
      |> Clg.group 
-- TODO amounts' = reduce amounts to greatest common denom
-- TODO total    = total = greatest common factor of amounts' (also >= minCircleSegments)
-- TODO clean up


wedge : Float -> Float -> Int -> Clg.Shape
wedge aStart aOff amt = 
  let calcAngle i = 
        aStart + toFloat i * aOff
      angles = 
        List.map calcAngle [ 0 .. amt ]
      outerPoints =
        List.map (curry fromPolar radius) angles
  in  
    Clg.polygon <| 
      (0,0) :: outerPoints


-- CONSTANTS

radius : Float
radius = 1

minCircleSegments : Int
minCircleSegments = 60


-- CONVENIENCE FUNCTIONS
{-
pctPieChart : (Int, Int) -> (Color, Color) -> Clg.Form
pctPieChart (numer, denom) (nClr, dClr) =
  pieChart [ numer, denom - numer ]
           [ nClr, dClr ]
-}


-- 

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


-- TODO move this to some Math module
greatestCommonFactor : Int -> List Int -> Int
greatestCommonFactor n factors =
  n -- TODO

-- from circuithub/elm-list-extra
-- TODO install that package
{-| Return all initial segments of a list, from shortest to longest, 
empty list first, the list itself last.
    
    inits [1,2,3] == [[],[1],[1,2],[1,2,3]]
-}
inits : List a -> List (List a)
inits = List.foldr (\e acc -> []::List.map ((::)e) acc) [[]]
