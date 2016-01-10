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
      `2 and 3 4ths`, i.e. `2 + (3 / 4)`
    contains `2` "wholes", and an "overflow" of the fraction `3 over 4`.
-}
type alias MixedNumber =
  { wholes : Int
  , overflow : Overflow
  }


{-| The fractional component of a MixedNumber. Represents whatever 
amount remains after the Wholes component. 

A MixedNumber that represents an exact integer value will have an
overflow of `Zero`, whereas a MixedNumber that represents some
integer value plus "a little bit more" will have an overflow of 
`ATadMore`, with an associated Fraction.
-}
type Overflow
  = Zero 
  | ATadMore Fraction
