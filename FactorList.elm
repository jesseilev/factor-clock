module FactorList where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import String
import Result.Extra as ResEx
import Json.Decode as Json


--------
-- MODEL
--------


type alias Model =
  { factors : List Int }


init : List Int -> Model 
init factors = 
  { factors = factors }



---------
-- UPDATE
---------


type Action
  = SetFactors (List Int)


update : Action -> Model -> Model
update action model =
  case action of
    SetFactors factors ->
      { model |
          factors = factors 
      }
        |> Debug.watch "fl model"



-------
-- VIEW
-------


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
    [ Html.input
        [ Attr.placeholder "comma separated numbers"
        , Attr.value (stringFromFactors model.factors)
        , Events.on "input" Events.targetValue (handleInput address)
        --, onEnter address
        , inputStyle
        ] 
        []
    ]


-- VIEW HELPERS

{-
onEnter : Signal.Address Action -> Html.Attribute
onEnter address =
  Events.on "keydown"
    (Json.customDecoder Events.keyCode is13)
    (handleInput address)
-}


handleInput : Signal.Address Action -> String -> Signal.Message
handleInput address str =
  let action = (SetFactors <| factorsFromString str)
                  |> Debug.watch "action"
  in 
    Signal.message address action


stringFromFactors : List Int -> String
stringFromFactors =
  List.map toString
    >> String.join ","
    >> flip String.append ","


factorsFromString : String -> List Int
factorsFromString =
  String.split "," 
    >> List.map String.toInt
    >> List.filter ResEx.isOk
    >> ResEx.combine
    >> ResEx.extract (\e -> [])



is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"



-- STYLES

inputStyle : Html.Attribute
inputStyle =
  Attr.style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]