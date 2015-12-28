import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg exposing (collage)
import Color                   exposing (..)
import Mouse
import Time

import NestedFraction          exposing (..)
import PieChart                exposing (pieChart)
import Clock as Clock

main : Signal Element
main = 
  Signal.map clockWithLabel stateSig


clockWithLabel : Clock.Model -> Element
clockWithLabel model = 
  let label = Elt.show <| model
      clock = 
        collage 700 700
          <| [ Clg.scale 300 (isoView Clock.view model)
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
      let hue p = toFloat (p % 700) / 700
      in 
        Clock.SetHues (hue x, hue y)


countTick : Signal Int
countTick = 
  Signal.foldp (\tick count -> count + 1) 0 (Time.every 1000)

factors = [3,2,5,2]
hues = 
  (0.6, 0.5)


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