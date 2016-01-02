module NestedFractionVisualization where

import Graphics.Collage as Clg
import Color exposing (..)

import NestedFraction as NF
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
  let 
    n = model.nestedFraction.numer
    d = model.nestedFraction.denom
    futureAmt = d - n.wholes
    amounts = [ n.wholes
              , (min 1 futureAmt) 
              , (futureAmt - 1)
              ] 
    pieModel = 
      Pie.init amounts (colors model.hues)
    pieAddress =
      Signal.forwardTo address (\a -> PieEvent a)
    parentView = Pie.view pieAddress pieModel
  in 
    Clg.group <| 
      [ parentView
      , presentChildView address model
          |> circlePackTransform n.wholes d
      ]



presentChildView : Signal.Address Action -> Model -> Clg.Form
presentChildView address model = 
  let 
    presentChildModel = 
      case model.nestedFraction.numer.overflow of 
        NF.Zero ->
          Nothing 
        NF.Fraction nf ->
          Just <| init nf model.hues
    -- TODO address?
  in
    case presentChildModel of 
      Nothing ->
        empty
      Just pcm -> 
        view address pcm


{-| The transform necessary to circle-pack a child circle
inside a larger parentView circle.
  -- TODO move to PieChart or related module
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
  [ hsla (turns <| h1) 0.6 0.3 1 -- less than
  , hsla (turns <| h2) 1 0.5 1 -- equal
  , hsla 0 0 0.5 1 -- greater than
  ] 


empty : Clg.Form
empty = Clg.circle 0 |> Clg.filled black


radius : Float
radius = 1
