module Clock where

import Time as Time exposing (Time)
import Graphics.Collage as Clg

import NestedFraction  as NF exposing (NestedFraction)
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


nestedFraction : Model -> NestedFraction
nestedFraction model = 
  let mixedNum = NF.fromDivision model.tick model.denoms
      str = Debug.watch "mixedNum" (toString mixedNum)

  in 
    NF.getFraction mixedNum.overflow


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

