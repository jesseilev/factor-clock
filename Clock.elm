module Clock where

import Time as Time exposing (Time)
import Graphics.Collage as Clg

import Nested
import Nested.MixedNumber as MixedNumber
import Nested.Fraction as NF
import NestedFractionVisualization as NFV

-- MODEL

type alias Model = 
  { denoms : List Int
  , tick : Int 
  , cycleDuration : Time
  , hues : (Float, Float)
  }


init : List Int -> Time -> (Float, Float) -> Model
init denoms compCy hues = 
  { denoms = denoms
  , tick = 0
  , cycleDuration = compCy
  , hues = hues
  }


nestedFraction : Model -> Nested.Fraction
nestedFraction model = 
  let mixedNum = MixedNumber.fromDivision model.tick model.denoms
      str = Debug.watch "mixedNum" (toString mixedNum)

  in 
    mixedNum.overflow |> NF.fractionElse0Over1


-- UPDATE 

type Action 
  = SetDenoms (List Int)
  | IncTick
  | SetHues (Float,Float)
  | VizEvent NFV.Action 


update : Action -> Model -> Model
update action model =
  case action of 
    SetDenoms ds ->
      { model | 
          denoms = ds 
      }

    IncTick ->
      { model | 
          tick = model.tick + 1 
      }

    SetHues hues ->
      { model | 
        hues = hues 
      }

    VizEvent vizAction -> 
      model


-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  let 
    nfvModel =
      NFV.init (nestedFraction model) model.hues
    nfvAdress =
      Signal.forwardTo address (\a -> VizEvent a)
  in 
    NFV.view nfvAdress nfvModel

