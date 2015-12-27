module Clock where

import Time as Time exposing (Time)
import Graphics.Collage as Clg

import NestedFraction  as NF exposing (NFraction)
import NFractComponent as NFC


-- MODEL

type alias Model = 
  { denoms : List Int
  , tick : Int 
  , completeCycle : Time
  , hues : (Float, Float)
  }


init : List Int -> Time -> (Float, Float) -> Model
init denoms compCy hues = 
  { denoms = denoms
  , tick = 0
  , completeCycle = compCy
  , hues = hues
  }


nfract : Model -> NFraction
nfract model = 
  NF.nestDiv model.denoms model.tick


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
      { model | denoms = ds }

    IncTick ->
      { model | tick = model.tick + 1 }

    SetHues hues ->
      { model | hues = hues }

    _ -> 
      model

-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  let 
    nfcModel =
      { nfract = (nfract model)
      , hues = model.hues
      }
  in 
    NFC.view (Signal.forwardTo address noop) nfcModel

noop : a -> Action
noop _ = NoOp
