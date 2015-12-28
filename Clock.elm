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
  NF.fromDivision model.denoms model.tick


-- UPDATE 

type Action 
  = SetDenoms (List Int)
  | IncTick
  | SetHues (Float,Float)
  | NoOp


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

    _ -> 
      model


-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  let 
    nfvModel =
      { nestedFraction = (nestedFraction model)
      , hues = model.hues
      }
    nfvAdress =
      Signal.forwardTo address (\_ -> NoOp)
  in 
    NFV.view nfvAdress nfvModel


noop : a -> Action
noop _ = NoOp
