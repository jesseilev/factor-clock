import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg exposing (collage)
import Html exposing (Html)
import Color exposing (..)
import Mouse
import Time exposing (Time)
import StartApp
import Effects exposing (Effects)

import ClockRecomposer as CR
import Clock


app = 
  StartApp.start 
    { init = initState
    , inputs = [actionSig]
    , update = CR.update
    , view = CR.view 
    }


main : Signal Html
main = 
  app.html



-- STATE

initState : (CR.Model, Effects CR.Action)
initState = 
  let clockModel = Clock.init factors Time.hour hues
  in 
    CR.init clockModel


-- INPUTS 

actionSig : Signal CR.Action
actionSig =
  Signal.map handleUpdate inputs


handleUpdate : Update -> CR.Action
handleUpdate update =
  case update of 
    TimeTick ->
      CR.ClockUpdate Clock.IncTick
    MouseMove (x,y) ->
      let hue p = toFloat (p % dim) / toFloat dim
      in 
        CR.ClockUpdate <| Clock.SetHues (hue x, hue y)


inputs : Signal Update
inputs = 
  Signal.merge
    (Signal.map MouseMove Mouse.position)
    (Signal.sampleOn tickSig (Signal.constant TimeTick))


type Update 
  = MouseMove (Int, Int) 
  | TimeTick


tickSig : Signal ()
tickSig = 
  Mouse.clicks
  --Signal.sampleOn (Time.every tickDuration) (Signal.constant ())


-- CONFIGS

tickDuration : Float 
tickDuration = 
  250

factors : List Int
factors = [3,5,2,2,3]

hues : (Float, Float)
hues = 
  (0.6, 0.5)

dim : Int
dim = 700



{-| For debugging, when you don't care about addresses. 
Creates a "disconnected" view function that just takes a Model argument.
-}
-- TODO move elsewhere
isoView : (Signal.Address a -> m -> v) 
       -> (m -> v) 
isoView viewFunc =
  let nowhere =
    Signal.forwardTo (Signal.mailbox Nothing).address Just
  in
    viewFunc nowhere