import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg exposing (collage)
import Html exposing (Html)
import Color exposing (..)
import Mouse
import Time

import ClockRecomposer as CR
import Clock



main : Signal Html
main = 
  Signal.map renderCR stateSig


renderCR : CR.Model -> Html
renderCR model = 
  isoView CR.view model
  


-- STATE


stateSig : Signal CR.Model
stateSig = 
  Signal.foldp CR.update initState crActions


initState : CR.Model
initState = 
  let clockModel = Clock.init factors Time.hour hues
  in 
    CR.init clockModel




-- UPDATES 


crActions : Signal CR.Action
crActions =
  Signal.map handleUpdate updates


handleUpdate : Update -> CR.Action
handleUpdate update =
  case update of 
    TimeTick ->
      CR.ClockUpdate Clock.IncTick
    MouseMove (x,y) ->
      let hue p = toFloat (p % dim) / toFloat dim
      in 
        CR.ClockUpdate <| Clock.SetHues (hue x, hue y)


updates : Signal Update
updates = 
  Signal.merge
    (Signal.map MouseMove Mouse.position)
    (Signal.sampleOn tickSig <| Signal.constant TimeTick)


type Update 
  = MouseMove (Int, Int) 
  | TimeTick


tickSig : Signal ()
tickSig = 
  Mouse.clicks 
  --Time.every 200


-- CONFIGS

factors : List Int
factors = [3,2,5,2,2,3]

hues : (Float, Float)
hues = 
  (0.6, 0.5)

dim : Int
dim = 700



{-| For debugging. Creates a "disconnected" view function that 
doesn't need an address argument.
-}
-- TODO move elsewhere
isoView : (Signal.Address a -> b -> c) 
       -> (b -> c) 
isoView viewFunc =
  let nowhere =
    Signal.forwardTo (Signal.mailbox Nothing).address Just
  in
    viewFunc nowhere