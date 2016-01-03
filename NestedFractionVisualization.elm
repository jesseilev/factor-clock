module NestedFractionVisualization where

import Graphics.Collage as Clg
import Color exposing (..)

import NestedFraction as NF  exposing (floor, rem)
import PieChart       as Pie exposing (view)

-- MODEL 

type alias Model =
  { nestedFraction : NF.NestedFraction
  , hues : (Float, Float)
  }


init : NF.NestedFraction -> (Float, Float) -> Model
init nf hues = 
  { nestedFraction = nf 
  , hues = hues
  }


-- UPDATE

type Action 
  = SetNestedFraction NF.NestedFraction
  | SetHues (Float, Float)
  | PieEvent Pie.Action


update : Action -> Model -> Model
update action model =
  case action of 
    SetHues hues ->
      { model | 
          hues = hues 
      }
    SetNestedFraction nf ->
      { model | 
          nestedFraction = nf 
      }
    PieEvent a ->
      model


-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  case model.nestedFraction of
    NF.Whole w -> 
      Clg.circle radius |> Clg.filled (color1 model.hues)
    NF.Nested w n d ->
      let 
        (nFloor, nRem) = (NF.floor n, NF.rem n)
        amounts = [ nFloor
                  , (min 1 (d - nFloor)) 
                  , (d - nFloor - 1)
                  ] 
        pieModel = 
          Pie.init amounts (colors model.hues)
        pieAddress =
          Signal.forwardTo address (\a -> PieEvent a)
        parentView = Pie.view pieAddress pieModel
        
        presentChildModel = init nRem model.hues
        -- TODO presentChildAddress?
        presentChild = 
          if nFloor < d then
            view address presentChildModel
          else 
            empty
        
        pastChildModel = 
          { model | 
              nestedFraction = (past model.nestedFraction) 
          }
        pastChildView = view address pastChildModel
        pastChildViews = 
          List.map2 (flip circlePackTransform d) 
            [0..nFloor] (List.repeat nFloor pastChildView)
      in 
        Clg.group <| 
          [ parentView
          , presentChild |> circlePackTransform nFloor d
          ] 
          ++ pastChildViews


{-|   1 + (2         + 1/3) / 5
      1 + (3/3 + 3/3 + 1/3) / 5

      past 14 / [3,5] = 4 * (3 / [3])
      past 14 / [5,3] = 2 * (5 / [5])

      14 / [3,5] 
      0 + (4 + (2/3)) / 5
-}
past : NF.NestedFraction -> NF.NestedFraction
past nf = 
  case nf of
    NF.Whole w -> 
      NF.Whole w
    NF.Nested _ n _ -> 
      NF.one n


{-| The affine transform for circle-packing a child circle
inside a larger parent circle.
-}
circlePackTransform : Int -> Int -> Clg.Form -> Clg.Form
circlePackTransform numer denom =
  let bigAng = turns (1 / toFloat denom)
      lilAng = bigAng / 2
      st     = sin (lilAng)
      lilRad = radius * st / (st + 1)
      dist   = radius - lilRad
      scale  = lilRad / radius
      rot    = toFloat numer * bigAng
      move   = fromPolar (dist, rot + lilAng)
      extraRot = lilAng + (turns 0.5)
  in  
  -- TODO factor out scale, which is numerator agnositc
    Clg.move move 
    << Clg.rotate (rot + extraRot) 
    << Clg.scale (scale * 0.9)


{-| Given hues, make some colors.
-}
colors : (Float, Float) -> List Color
colors (h1, h2) =
  [ hsla (turns <| h1) 0.6 0.3 0.12 -- less than
  , hsla (turns <| h2) 1 0.5 0.25 -- equal
  ,   hsla 0 0 0.5 0.3 -- greater than
  ] 


color1 : (Float,Float) -> Color
color1 model =
  let clrs = colors model
  in 
    case List.head clrs of
      Just c -> c
      Nothing -> black


empty : Clg.Form
empty = Clg.circle 0 |> Clg.filled black


radius : Float
radius = 1
