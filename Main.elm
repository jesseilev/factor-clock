import Graphics.Element as Elt exposing (Element)
import Graphics.Collage as Clg exposing (collage)
import Color                   exposing (..)
import Mouse
import Time
--import StartApp.Simple         exposing (start)

import NestedFraction          exposing (..)
import PieChart                exposing (pieChart)
import NFractComponent as NFC 


main : Signal Element
main = 
  Signal.map clockWithLabel stateSig

clockWithLabel : NFC.Model -> Element
clockWithLabel model = 
  let label = Elt.show <| model
      pie   = collage 700 700
                <| [ NFC.isoView model 
                       |> Clg.scale 300
                   ]
  in 
    Elt.flow Elt.down [pie, label]

stateSig : Signal NFC.Model
stateSig = Signal.foldp NFC.update initState nfActionSig

initState : NFC.Model
initState = NFC.init (nestDiv factors 0) hues

nfActionSig : Signal NFC.Action
nfActionSig =
  Signal.map handleUpdate updates

handleUpdate u =
  case u of 
    TimeTick tick ->
      NFC.SetNFract <| makeNF tick
    MouseMove (x,y) ->
      let hue p = toFloat (p % 700) / 700
      in 
        NFC.SetHues (hue x, hue y)

countTick : Signal Int
countTick = 
  Signal.foldp (\tick count -> count + 1) 0 (Time.every 100)

makeNF : Int -> NFraction
makeNF = nestDiv factors

factors = [3,5,2,3,3,2,5]
hues = (0.0, 0.5)

type Update = MouseMove (Int, Int) | TimeTick Int

updates : Signal Update
updates = 
  Signal.merge
    (Signal.map MouseMove Mouse.position)
    (Signal.map TimeTick countTick)