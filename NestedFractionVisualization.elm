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
      model -- TODO


-- VIEW

getNumerDenom : Model -> (Int, Int)
getNumerDenom model =
  (model.nestedFraction.numer.wholes, model.nestedFraction.denom)

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  let 
    (nWholes, denom) = getNumerDenom model
    futureAmt = denom - nWholes
    amounts = [ nWholes
              , (min 1 futureAmt) 
              , (futureAmt - 1)
              ] 
    pieModel = 
      Pie.init amounts (colors model.hues)
    pieAddress =
      Signal.forwardTo address PieEvent
    parentView = Pie.view pieAddress pieModel

  in 
    Clg.group <| 
      [ parentView
      , presentChildView address model
          |> circlePackTransform nWholes denom
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
  in
    case presentChildModel of 
      Nothing ->
        empty
      Just pcm -> 
        view address pcm
        -- TODO child address?


pastChildViews : Signal.Address Action -> Model -> List Clg.Form
pastChildViews address model =
  let 
    pastChildModel =
      { model | 
          nestedFraction = (past model.nestedFraction) 
      }
    pcView = view address pastChildModel
    (nWholes, denom) = getNumerDenom model
    transform = (flip circlePackTransform) denom
  in
    List.map2 transform [0..nWholes] (List.repeat nWholes pcView)


past : NF.NestedFraction -> NF.NestedFraction
past nf =
  (NF.zero (nf.numer)).overflow |> NF.getFraction


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
    Clg.move move 
    << Clg.rotate (rot + lilAng) 
    << Clg.scale (scale * 0.85)


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
