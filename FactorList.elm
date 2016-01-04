module FactorList where

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import String
import Result.Extra as ResEx



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
        , inputStyle
        ] 
        []
    ]


-- VIEW HELPERS


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


factorsFromString : String -> List Int
factorsFromString =
  String.split "," 
    >> List.map String.toInt
    >> List.filter ResEx.isOk
    >> List.map (Result.map ((*) 2)) -- (debugging. remove)
    >> ResEx.combine
    >> ResEx.extract (\e -> [])


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