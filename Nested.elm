module Nested where 


{-| A recursively-defined fraction, kind of like a Continued Fraction,
but with a few differences:

  1. the numerator is recursive, rather than the denominator
  2. the non-recursive element (the denominator) can be any integer
value, not just 1
-}
type alias Fraction =
  { numer : MixedNumber
  , denom : Int 
  }

{-| Represents a fraction with an integral, or "whole numbers" component.

    For example, the value:
      2 and 3 4ths, i.e. 2 + (3 / 4)
    contains 2 "wholes", and an "overflow" of the fraction 3 over 4.
-}
type alias MixedNumber =
  { wholes : Int
  , overflow : Overflow
  }


type Overflow
  = Zero 
  | ATadMore Fraction
