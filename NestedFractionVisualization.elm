module NestedFractionVisualization where

import Graphics.Collage as Clg
import Color exposing (..)

import Nested 
import Nested.MixedNumber as MixedNumber
import Nested.Fraction as NF
import PieChart as Pie exposing (view)
import Factorization exposing (Factorization)
import FactorizationDisplay as FD
import Circle


-- MODEL 

type alias Model =
  { nestedFraction : Nested.Fraction
  , hues : (Float, Float)
  }


init : Nested.Fraction -> (Float, Float) -> Model
init nf hues = 
  { nestedFraction = nf 
  , hues = hues
  }


getNumerDenom : Model -> (Int, Int)
getNumerDenom model =
  ( model.nestedFraction.numer.wholes
  , model.nestedFraction.denom
  )


{-| Given hues, make some colors.
-}
colors : (Float, Float) -> (Color, Color, Color)
colors (h1, h2) =
  ( hsla (turns h1) 0.5 0.25 1 -- less than
  , hsla (turns h2) 1 0.5 0.5 -- equal
  , hsla 0 0 0.85 1 -- greater than
  ) 


-- UPDATE

type Action 
  = SetNestedFraction Nested.Fraction
  | SetHues (Float, Float)
  | PieEvent Pie.Action
  | ChildFDEvent FD.Action


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
    _ ->
      model -- TODO


-- VIEW


view : Signal.Address Action -> Model -> Clg.Form
view address model =
  let 
    (nWholes, denom) = 
      getNumerDenom model
    futureAmt = 
      denom - nWholes
    amounts = 
      [ nWholes
      , (min 1 futureAmt) 
      , (futureAmt - 1)
      --, futureAmt
      ] 
    pieModel = 
      Pie.init amounts ( colors model.hues |> \(x,y,z) -> [x,y,z] )
    pieAddress =
      Signal.forwardTo address PieEvent
    parentView = Pie.view pieAddress pieModel

    childDenoms =
      MixedNumber.denoms (MixedNumber.fromFraction model.nestedFraction)
        |> List.tail
    mChildFDView = 
      Maybe.map (childFDView address) childDenoms
    repeatChildFD n =
      Maybe.withDefault []
        <| Maybe.map (List.repeat n) mChildFDView
    lessThanViews =
      repeatChildFD nWholes
    greaterThanViews =
      repeatChildFD (denom - nWholes)
    childViews =
      lessThanViews ++
      [ childNFView address model ]
      ++ greaterThanViews
  in 
    Clg.group <| 
      parentView
      :: (childViews |> circlePackChildren denom)
      --++ (pastChildViews address model)
      --++ (futureChildViews address model)


{-| If model.nestedFraction contains some nonzero Overflow, return a
NestedFractionVizualization for that Overflow. Otherwise, Overflow is
Zero, the fraction is "flat", and there is no recursive child, so
return empty
-- TODO return Nothing instead?
-}

childNFView : Signal.Address Action -> Model -> Clg.Form
childNFView address model =
  let 
    maybeFraction =
      NF.childFraction model.nestedFraction
    maybeModel = 
      Maybe.map (flip init model.hues) maybeFraction
    maybeView = 
      Maybe.map (view address) maybeModel
    (clrLT, clrEQ, clrGT) =
      colors model.hues
    leafView =
      Clg.circle 1 |> Clg.filled clrEQ
  in 
    Maybe.withDefault leafView maybeView
    -- TODO child address?


childFDView : Signal.Address Action -> List Int -> Clg.Form
childFDView address childDenoms =
  let 
    childModel =
      FD.init childDenoms (\_ -> hsla 0 0 0.5 0.25)
    childAddress =
      Signal.forwardTo address ChildFDEvent
  in 
    FD.view childAddress childModel


circlePackChildren : Int -> List Clg.Form -> List Clg.Form
circlePackChildren total childViews =
  let 
    transform = 
      Circle.circlePackTransform total
  in 
    List.map2 transform [0 .. total-1] childViews



emptyView : Clg.Form
emptyView = Clg.circle 0 |> Clg.filled black

