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
    (nWholes, denom) = getNumerDenom model
    futureAmt = denom - nWholes
    amounts = [ nWholes
             -- , (min 1 futureAmt) 
             -- , (futureAmt - 1)
              , futureAmt
              ] 
    pieModel = 
      Pie.init amounts (colors model.hues |> listFromPair)
    pieAddress =
      Signal.forwardTo address PieEvent
    parentView = Pie.view pieAddress pieModel

    mChildView = 
      maybeChildFDView address model 
    childFactorizationViews first last =
      Maybe.withDefault []
        <| Maybe.map (circleRepeat denom first last) mChildView
    lessThanViews =
      childFactorizationViews 0 nWholes
    greaterThanViews =
      childFactorizationViews (nWholes + 1) denom 

  in 
    Clg.group <| 
      [ parentView
      , presentChildView address model
          |> Circle.circlePackTransform denom nWholes
      ]
      ++ lessThanViews
      ++ greaterThanViews
      --++ (pastChildViews address model)
      --++ (futureChildViews address model)


maybeChildFDView : Signal.Address Action -> Model -> Maybe Clg.Form
maybeChildFDView address model =
  let 
    denoms =
      MixedNumber.denoms (MixedNumber.fromFraction model.nestedFraction)
    maybeChildDenoms =
      List.tail denoms 
    maybeChildModel =
      Maybe.map2 FD.init maybeChildDenoms (Just <| \_ -> hsla 0 0 0.5 0.5)
    childAddress =
      Signal.forwardTo address ChildFDEvent
  in 
    Maybe.map (FD.view childAddress) maybeChildModel


circleRepeat : Int -> Int -> Int -> Clg.Form -> List Clg.Form
circleRepeat total first last childView =
  let 
    repeats =
      List.repeat (last - first) childView
    transform = 
      Circle.circlePackTransform total
  in 
    List.map2 transform [first .. last+1] repeats





----


type When 
  = Past 
  | Present
  | Future


newChildFraction : When -> Nested.Fraction -> Nested.Fraction
newChildFraction when =
  case when of 
    Present ->
      identity 
    Past ->
      NF.full
    Future ->
      NF.empty


childView : When -> Signal.Address Action -> Model -> Clg.Form
childView when address model =
  let 
    maybeFraction =
      NF.childFraction model.nestedFraction
    newFraction = 
      Maybe.map (newChildFraction when) maybeFraction
    maybeModel = 
      Maybe.map (flip init model.hues) newFraction
    maybeView = 
      Maybe.map (view address) maybeModel
  in 
    Maybe.withDefault (leafView when address model) maybeView
    -- TODO child address?

leafView : When -> Signal.Address Action -> Model -> Clg.Form
leafView when address model =
  let 
    clrs = colors model.hues
    clr = 
      case when of
        Past -> fst clrs
        Future -> snd clrs
        Present -> snd clrs
  in 
    Clg.circle 1 |> Clg.filled clr


{-| If model.nestedFraction contains some nonzero Overflow, return a
NestedFractionVizualization for that Overflow. Otherwise, Overflow is
Zero, the fraction is "flat", and there is no recursive child, so
return empty
-- TODO return Nothing instead?
-}
presentChildView : Signal.Address Action -> Model -> Clg.Form
presentChildView address model = 
  if isBetween0And1 model.nestedFraction then
    childView Present address model
  else 
    emptyView


isBetween0And1 : Nested.Fraction -> Bool
isBetween0And1 fract =
  let 
    childCheck =
      Maybe.map isBetween0And1 (NF.childFraction fract)
    greater = 
      fract.numer.wholes > 0 || Maybe.withDefault False childCheck
    less = fract.numer.wholes < fract.denom
  in
    greater && less


pastChildViews : Signal.Address Action -> Model -> List Clg.Form
pastChildViews address model =
  let 
    pcView = childView Past address model
    (nWholes, denom) = getNumerDenom model
    transform = Circle.circlePackTransform denom
  in
    List.map2 transform [0..nWholes-1] (List.repeat nWholes pcView)


futureChildViews : Signal.Address Action -> Model -> List Clg.Form
futureChildViews address model =
  let 
    fcView = childView Future address model 
    (nWholes, denom) = getNumerDenom model 
    amount = denom - nWholes |> Debug.watch "num futures"
    transform = Circle.circlePackTransform denom
  in 
    List.map2 transform [nWholes+1 .. denom-1] (List.repeat amount fcView)



getNumerDenom : Model -> (Int, Int)
getNumerDenom model =
  (model.nestedFraction.numer.wholes, model.nestedFraction.denom)



{-| Given hues, make some colors.
-}
colors : (Float, Float) -> (Color, Color)
colors (h1, h2) =
  ( hsla (turns <| h1) 1 0.15 1.5 -- less than
  , hsla (turns <| h2) 0.7 0.75 0.5 -- equal
  ) 


listFromPair : (a,a) -> List a
listFromPair (x,y) =
  [x,y]


emptyView : Clg.Form
emptyView = Clg.circle 0 |> Clg.filled black

