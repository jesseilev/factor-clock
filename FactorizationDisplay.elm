module FactorizationDisplay where


import Graphics.Collage as Clg 
import Color exposing (..)

import Factorization exposing (Factorization)
import Circle


-- MODEL

type alias Model = 
  { factorization : Factorization 
  , decideColor : Int -> Color
  }


init : Factorization -> (Int -> Color) -> Model 
init = Model


-- UPDATE

type Action
  = SetFactorization Factorization
  | ChildEvent Action


update : Action -> Model -> Model
update action model =
  case action of
    SetFactorization factorization ->
      { model 
          | factorization = factorization
      }
    _ ->
      model



-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  let 
    maybeFirstFactor = 
      List.head model.factorization
    color = 
      model.decideColor (Maybe.withDefault 0 maybeFirstFactor)
    parent = 
      Clg.circle 1 |> Clg.filled color
    maybeChildModel =
      Maybe.map (flip init model.decideColor) (List.tail model.factorization)
    createChildView =
      view (Signal.forwardTo address ChildEvent)
    maybeChildView =
      Maybe.map createChildView maybeChildModel
    maybeRepeatedViews =
      Maybe.map2 circleRepeat maybeFirstFactor maybeChildView
    children = 
      Maybe.withDefault [] maybeRepeatedViews 
  in 
    Clg.group <|
      parent :: children


circleRepeat : Int -> Clg.Form -> List Clg.Form
circleRepeat n childView =
  List.map2 
    (Circle.circlePackTransform n) 
    [0 .. n-1] 
    (List.repeat n childView)

