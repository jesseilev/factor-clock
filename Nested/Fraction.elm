module Nested.Fraction where


import Nested exposing (Fraction, MixedNumber, Overflow)


maybeFraction : Overflow -> Maybe Nested.Fraction
maybeFraction overflow =
  case overflow of 
    Nested.Zero ->
      Nothing
    Nested.ATadMore fract ->
      Just fract


fractionWithDefault : Fraction -> Overflow -> Nested.Fraction
fractionWithDefault default =   
  Maybe.withDefault default << maybeFraction


fractionElse0Over1 : Overflow -> Fraction
fractionElse0Over1 =
  fractionWithDefault mempty


mempty : Nested.Fraction
mempty = 
  Nested.Fraction (mnFromWholes 0) 1


mnFromWholes n = 
  MixedNumber n Nested.Zero


childFraction : Fraction -> Maybe Fraction
childFraction nf =
  nf.numer.overflow |> maybeFraction


empty : Fraction -> Fraction
empty nf =
  let 
    newNumer = 
      case nf.numer.overflow of 
        Nested.Zero ->
          mnFromWholes 0
        Nested.ATadMore childNF ->
          MixedNumber 0 (Nested.ATadMore (empty childNF))
  in 
    { nf |
        numer = newNumer
    } 

full : Fraction -> Fraction
full nf =
  let 
    newNumer = 
      case nf.numer.overflow of
        Nested.Zero ->
          mnFromWholes nf.denom
        Nested.ATadMore childNF ->
          MixedNumber nf.denom (Nested.ATadMore (empty childNF))
    in 
      { nf |
          numer = newNumer
      } 


compress : MixedNumber -> Fraction
compress mn = 
  fractionElse0Over1 mn.overflow -- TODO

isReduced : Fraction -> Bool
isReduced nf =
  let rootIsReduced = 
        nf.numer.wholes < nf.denom 
      childIsReduced =
        Maybe.withDefault True 
          (Maybe.map isReduced <| childFraction nf)
  in 
    rootIsReduced && childIsReduced
