module FactorList where

import Html exposing (Html)
import Html.Attributes as Attr
import String
import Result.Extra as ResEx

-- MODEL

type alias Model =
  { factors : List Int }


init : List Int -> Model 
init factors = 
  { factors = factors }



-- UPDATE

type Action
  = SetFactors (List Int)


update : Action -> Model -> Model
update action model =
  case action of
    SetFactors factors ->
      { model |
          factors = factors 
      }



-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
    [ Html.input
        [ Attr.placeholder "comma separated numbers"
        , Attr.value (model.factors |> toString)
        ] 
        []
    ]



toIntList : String -> List (Result String Int)
toIntList str =
  str
    |> String.split "," 
    |> List.map String.toInt
    |> List.filter ResEx.isOk

