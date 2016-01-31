module ClockRecomposer where

import Graphics.Collage as Clg
import Html exposing (Html)
import Html.Attributes as Attr exposing (..)
import Effects exposing (Effects)
import Color exposing (..)

import FactorList as FL
import Clock


-- MODEL

type alias Model =
  { factorList : FL.Model
  , clock : Clock.Model
  }


init : Clock.Model -> (Model, Effects Action)
init clockModel =
  ( { factorList = FL.init clockModel.denoms
    , clock = clockModel
    }
  , Effects.none
  )


defaultFactors = [5,3,2]


-- UPDATE 


type Action 
  = FactorListUpdate FL.Action
  | ClockUpdate Clock.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  let newModel = 
    case action of 
      FactorListUpdate a ->
        case a of 
          FL.SetFactors fs ->
            { model | 
                factorList = FL.update a model.factorList
            ,   clock = Clock.update (Clock.SetDenoms fs) model.clock
            }
      ClockUpdate a ->
        { model |
            clock = Clock.update a model.clock
        }
  in 
    (newModel, Effects.none)

-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [ viewStyle ]
    [ clockView address model 
    , flView address model
    ]
  

flView : Signal.Address Action -> Model -> Html
flView address model = 
  FL.view 
    (Signal.forwardTo address FactorListUpdate)
    model.factorList


clockView : Signal.Address Action -> Model -> Html
clockView address model = 
  let clockAddress = 
        Signal.forwardTo address ClockUpdate
      clockForm = 
        Clock.view clockAddress model.clock
      background =
        Clg.circle 1 |> Clg.filled (hsl 0 0 0.5)
      scaleForms = 
        List.map (Clg.scale <| toFloat dim/2) 
      element = 
        Clg.collage dim dim 
          <| scaleForms [ background, clockForm ]
  in 
    Html.fromElement element


viewStyle : Html.Attribute
viewStyle =
  style
    []


-- TODO main? 
dim = 700
