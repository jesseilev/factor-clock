module NFractComponent where

import Graphics.Collage as Clg
import Color exposing (..)

import NestedFraction as NF exposing (..)
import PieChart             exposing (..)
import NumExtra             exposing (divf)


-- MODEL 

type alias Model =
  { nfract : NFraction
  , hues : (Float, Float)
  }

init : NFraction -> (Float, Float) -> Model
init nf hues = 
  { nfract = nf 
  , hues = hues
  }

-- UPDATE

type Action 
  = SetNFract NFraction
  | SetHues (Float, Float)

update : Action -> Model -> Model
update action model =
  case action of 
    SetHues hues ->
      { model | hues = hues }
    SetNFract nf ->
      { model | nfract = nf }

-- VIEW

view : Signal.Address Action -> Model -> Clg.Form
view address model =
  case model.nfract of
    (Whole w) -> 
      Clg.circle radius |> Clg.filled (color1 model)
    (Nested w n d) ->
      let 
        (nFloor, nRem) = (NF.floor n, NF.rem n)
        amounts = [ nFloor
                  , (min 1 (d - nFloor)) 
                  , (d - nFloor - 1)
                  ] 
        parent = pieChart amounts (colors model)
        presentChild = 
          if nFloor < d
            then view address presentChildModel
            else empty
        presentChildModel = { model | nfract = nRem }
        pastChildModel = { model | nfract = (past model.nfract) }
        pastChildView = view address pastChildModel
        pastChildViews = 
          List.map2 (flip circlePackTransform d) 
            [0..nFloor] (List.repeat nFloor pastChildView)
      in 
      Clg.group <| 
        [ parent
        , presentChild |> circlePackTransform nFloor d
        ] 
        ++ pastChildViews

{-|   1 + (2         + 1/3) / 5
      1 + (3/3 + 3/3 + 1/3) / 5

      past 14 / [3,5] = 4 * (3 / [3])
      past 14 / [5,3] = 2 * (5 / [5])

      14 / [3,5] 
      0 + (4 + (2/3)) / 5
-}
past : NFraction -> NFraction
past nf = case nf of
  (Whole w)      -> Whole w
  (Nested w n d) -> one n


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

colors : Model -> List Color
colors model =
  [ hsla (turns <| fst model.hues) 0.7 0.6 0.2
  , hsla (turns <| snd model.hues) 0.6 0.1 1
  , hsla 0 0 0.5 1
  ] 
   {-
  let (h1, _) = model.hues
        c s = hsla (turns h1) s 0.5 1
    in
    [ c 1, c 0.4, c 0.1 ]
  -}

color1 : Model -> Color
color1 model =
  let clrs = colors model
  in 
    case List.head clrs of
      Just c -> c
      Nothing -> black

empty : Clg.Form
empty = Clg.circle 0 |> Clg.filled black

radius : Float
radius = 1
