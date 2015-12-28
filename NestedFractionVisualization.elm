module NestedFractionVisualization where

import Graphics.Collage as Clg
import Color exposing (..)

import NestedFraction as NF exposing (..)
import PieChart             exposing (..)
import NumExtra             exposing (divf)


-- MODEL 

type alias Model =
  { nestedFraction : NestedFraction
  , hues : (Float, Float)
  }


init : NestedFraction -> (Float, Float) -> Model
init nf hues = 
  { nestedFraction = nf 
  , hues = hues
  }


-- UPDATE

type Action 
  = SetNestedFraction NestedFraction
  | SetHues (Float, Float)


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


-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  case model.nestedFraction of
    Whole w -> 
      Clg.circle radius |> Clg.filled (color1 model.hues)
    Nested w n d ->
      let 
        (nFloor, nRem) = (NF.floor n, NF.rem n)
        amounts = [ nFloor
                  , (min 1 (d - nFloor)) 
                  , (d - nFloor - 1)
                  ] 
        parent = pieChart amounts (colors model.hues)
        presentChild = 
          if nFloor < d then
            view address presentChildModel
          else 
            empty
        presentChildModel = 
          { model | 
              nestedFraction = nRem 
          }
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
        [ parent
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
past : NestedFraction -> NestedFraction
past nf = case nf of
  Whole w -> 
    Whole w
  Nested _ n _ -> 
    one n


{-| The transform necessary to circle-pack a child circle
inside a larger parent circle.
-}
circlePackTransform : Int -> Int -> Clg.Form -> Clg.Form
circlePackTransform numer denom =
  let lilAng = turns <| 0.5 * (1 / toFloat denom)
      st     = sin (lilAng)
      lilR   = radius * st / (st + 1)
      dist   = radius - lilR
      scale  = lilR / radius
      rot    = toFloat numer * (2 * lilAng)
      move   = fromPolar (dist, rot + lilAng)
  in  
  -- TODO factor out scale, which is numerator agnositc
    Clg.move move << Clg.rotate (rot + lilAng) << Clg.scale scale 


{-| Given hues, make some colors.
-}
colors : (Float, Float) -> List Color
colors (h1, h2) =
  [ hsla (turns <| h1) 0.7 0.6 0.3 -- less than
  , hsla (turns <| h2) 0.6 0.2 1 -- equal
  , hsla 0 0 0.6 1 -- greater than
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
