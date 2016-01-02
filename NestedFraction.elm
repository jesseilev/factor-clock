module NestedFraction where

{-| A recursively-defined fraction, kind of like a Continued Fraction,
but with a few differences:
  1. the numerator is recursive, rather than the denominator
  2. the non-recursive element (the denominator) can be any integer
value, not just 1
-}

type alias NestedFraction =
  { numer : MixedNumber
  , denom : Int 
  }

type alias MixedNumber =
  { wholes : Int
  , overflow : Overflow
  }

type Overflow
  = Zero 
  | Fraction NestedFraction


fromWholes : Int -> MixedNumber
fromWholes n = MixedNumber n Zero


withoutWholes : NestedFraction -> MixedNumber
withoutWholes = MixedNumber 0 << Fraction 


{-| Create an MixedNumber given a numerator and a 
list of denominators.
-}
fromDivision : Factorization -> Int -> MixedNumber
-- TODO flip args
fromDivision denoms n = 
  case denoms of
    [] -> 
      fromWholes n
    (d::ds) -> 
      let wholes = n // (product denoms)
          rem    = n %  (product denoms)
      in 
        add 
          (fromWholes wholes)
          (fromDivision ds rem)


{-| Addition operation between two NestedFractions.
-}
add : MixedNumber -> MixedNumber -> MixedNumber
add mn mn' = 
  let newMN = MixedNumber (mn.wholes + mn'.wholes)
  in 
    case (mn.overflow, mn'.overflow) of
      (Zero, Zero) -> 
        newMN Zero

      (Fraction fract, Zero) -> 
        newMN (Fraction fract)
      
      (Zero, Fraction fract') ->
        newMN (Fraction fract')

      (Fraction fract, Fraction fract') ->
        let 
          newNumer = 
            add
              (fract.numer  `mult` fromWholes (fract'.denom))
              (fract'.numer `mult` fromWholes (fract.denom) )
        in 
          newMN <| Fraction <|
            NestedFraction 
              newNumer
              (fract.denom * fract'.denom)


mult : MixedNumber -> MixedNumber -> MixedNumber
mult nf nf' =
  let newMN = MixedNumber (nf.wholes * nf'.wholes)
  in 
    case (nf.overflow, nf'.overflow) of
      (Zero, Zero) -> 
        newMN Zero

      (Fraction fract, Zero) -> 
        newMN <| Fraction
          { fract |
              numer = mult fract.numer (fromWholes nf'.wholes)
          }
          
      (Zero, Fraction fract') -> -- commutative case
        newMN <| Fraction
          { fract' |
              numer = mult fract'.numer (fromWholes nf.wholes)
          }

      (Fraction fract, Fraction fract') ->
        newMN <| Fraction <| 
          NestedFraction
            (mult fract.numer fract'.numer)
            (fract.denom * fract'.denom)


{-| Return a NestedFraction that equals exactly 0 but contains the same 
hierarchy of denominators as nf.
The numerators of the resulting NestedFraction will all be 0.
-}
zero : NestedFraction -> MixedNumber
zero nf =
  let mixedNum = withoutWholes nf
  in 
    fromDivision (denoms mixedNum) 0
      

{-| Return a NestedFraction that equals exactly 1, but contains
the same hierarchy of denominators as nf.
The numerators of the resulting NestedFraction will all be equal to
their correspoding denominators.
-}
one : NestedFraction -> MixedNumber
one nf = 
  let mixedNum = withoutWholes nf 
      ds = denoms mixedNum 
  in 
    fromDivision ds (product ds)


denoms : MixedNumber -> Factorization
denoms mn =
  case mn.overflow of 
    Zero ->
      []
    Fraction fract ->
      fract.denom :: (denoms fract.numer)
-- TODO implement using List.fold


getFraction : Overflow -> NestedFraction
getFraction overflow =
  case overflow of 
    Zero -> 
      NestedFraction (fromWholes 0) 1
    Fraction fract ->
      fract 


type alias Factorization = 
  List Int


product : Factorization -> Int 
product = List.product
