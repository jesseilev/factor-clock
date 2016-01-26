module Nested.MixedNumber where


import Nested exposing (MixedNumber, Overflow)
-- exposing (Zero, ATadMore)
import Factorization exposing (Factorization)


fromWholes : Int -> MixedNumber
fromWholes n = 
  MixedNumber n Nested.Zero


fromFraction : Nested.Fraction -> MixedNumber
fromFraction = 
  MixedNumber 0 << Nested.ATadMore 


over : MixedNumber -> Int -> Overflow
over numer = 
  Nested.ATadMore << Nested.Fraction numer


{-| Create an MixedNumber given a numerator and a 
list of denominators.
-}
fromDivision : Int -> Factorization -> MixedNumber
fromDivision n denoms = 
  case denoms of
    [] -> 
      fromWholes n
    (d::ds) -> 
      let wholes = n // (Factorization.product denoms)
          rem    = n %  (Factorization.product denoms)
          childNumer = fromDivision rem ds
          overflow = childNumer `over` d
      in 
        MixedNumber wholes overflow


{-| Addition operation between two NestedFractions.
-}
add : MixedNumber -> MixedNumber -> MixedNumber
add mn mn' = 
  let newMN = MixedNumber (mn.wholes + mn'.wholes)
      s  = Debug.watch "mn"  (toString mn)
      s' = Debug.watch "mn'" (toString mn')
      s'' = Debug.log(s ++ " " ++ s')
  in 
    case (mn.overflow, mn'.overflow) of
      (Nested.Zero, Nested.Zero) -> 
        newMN Nested.Zero

      (Nested.ATadMore fraction, Nested.Zero) -> 
        newMN (Nested.ATadMore fraction)
      
      (Nested.Zero, Nested.ATadMore fraction') ->
        newMN (Nested.ATadMore fraction')

      (Nested.ATadMore fraction, Nested.ATadMore fraction') ->
        let 
          newNumer = 
            add
              (fraction.numer  `mult` fromWholes (fraction'.denom))
              (fraction'.numer `mult` fromWholes (fraction.denom) )
        in 
          newMN <| Nested.ATadMore <|
            Nested.Fraction 
              newNumer
              (fraction.denom * fraction'.denom)


mult : MixedNumber -> MixedNumber -> MixedNumber
mult nf nf' =
  let newMN = MixedNumber (nf.wholes * nf'.wholes)
  in 
    case (nf.overflow, nf'.overflow) of
      (Nested.Zero, Nested.Zero) -> 
        newMN Nested.Zero

      (Nested.ATadMore fraction, Nested.Zero) -> 
        newMN <| Nested.ATadMore
          { fraction |
              numer = mult fraction.numer (fromWholes nf'.wholes)
          }
          
      (Nested.Zero, Nested.ATadMore fraction') -> -- commutative case
        newMN <| Nested.ATadMore
          { fraction' |
              numer = mult fraction'.numer (fromWholes nf.wholes)
          }

      (Nested.ATadMore fraction, Nested.ATadMore fraction') ->
        newMN <| Nested.ATadMore <| 
          Nested.Fraction
            (mult fraction.numer fraction'.numer)
            (fraction.denom * fraction'.denom)


{-| Return a NestedFraction that equals exactly 0 but contains the same 
hierarchy of denominators as nf.
The numerators of the resulting NestedFraction will all be 0.
-}
empty : MixedNumber -> MixedNumber
empty mn =
  percentFilled 0 (denoms mn)
      

{-| Return a NestedFraction that equals exactly 1, but contains
the same hierarchy of denominators as nf.
The numerators of the resulting NestedFraction will all be equal to
their correspoding denominators.
-}
full : MixedNumber -> MixedNumber
full mn = 
  percentFilled 1 (denoms mn) 



percentFilled : Float -> Factorization -> MixedNumber
percentFilled pct factors =
  let divisor = 
    pct * toFloat (Factorization.product factors) |> round
  in 
    fromDivision divisor factors


denoms : MixedNumber -> Factorization
denoms mn =
  case mn.overflow of 
    Nested.Zero ->
      []
    Nested.ATadMore fract ->
      fract.denom :: (denoms fract.numer)
-- TODO implement this and other recursives using List.fold
  



-- TODO move this into Factorization module
