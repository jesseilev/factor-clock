import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg exposing (collage)
import Color                   exposing (..)
import Mouse
import Time

import NestedFraction          exposing (..)
import Clock as Clock

main : Signal Element
main = 
  Signal.map clockWithLabel stateSig


clockWithLabel : Clock.Model -> Element
clockWithLabel model = 
  let label = Elt.show <| Clock.nestedFraction model
      clock = 
        collage dim dim
          <| [ Clg.scale (dim/2) (isoView Clock.view model)
             ]
  in 
    Elt.flow Elt.down [clock, label]


stateSig : Signal Clock.Model
stateSig = Signal.foldp Clock.update initState nfActionSig


initState : Clock.Model
initState = Clock.init factors Time.hour hues


nfActionSig : Signal Clock.Action
nfActionSig =
  Signal.map handleUpdate updates


handleUpdate u =
  case u of 
    TimeTick tick ->
      Clock.IncTick
    MouseMove (x,y) ->
      let hue p = toFloat (p % dim) / dim
      in 
        Clock.SetHues (hue x, hue y)


countTick : Signal Int
countTick = 
  Signal.foldp (\tick count -> count + 1) 0 timeSig

timeSig : Signal ()
timeSig = 
  Mouse.clicks 
  --Time.every 200

factors = [3,2,5,2,2,3]
hues = 
  (0.6, 0.5)
dim = 800

type Update 
  = MouseMove (Int, Int) 
  | TimeTick Int


updates : Signal Update
updates = 
  Signal.merge
    (Signal.map MouseMove Mouse.position)
    (Signal.map TimeTick countTick)




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