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



-- FIXME all the fromX functions return MixedNumber, so they
-- should be namespaced to a MixedNumber module, or renamed
-- to something else


fromWholes : Int -> MixedNumber
fromWholes n = MixedNumber n Zero


fromFraction : NestedFraction -> MixedNumber
fromFraction = MixedNumber 0 << Fraction 


{-| Create an MixedNumber given a numerator and a 
list of denominators.
-}
fromDivision : Int -> Factorization -> MixedNumber
fromDivision n denoms = 
  case denoms of
    [] -> 
      fromWholes n
    (d::ds) -> 
      let wholes = n // (product denoms) |> (Debug.watch "wholes")
          rem    = n %  (product denoms) |> (Debug.watch "rem")
      in 
        add 
          (fromWholes wholes)
          (fromFraction <|
             NestedFraction (fromDivision rem ds) d
          )


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
    pct * toFloat (product factors) |> round
  in 
    fromDivision divisor factors


mempty : NestedFraction
mempty = NestedFraction (fromWholes 0) 1


denoms : MixedNumber -> Factorization
denoms mn =
  case mn.overflow of 
    Zero ->
      []
    Fraction fract ->
      fract.denom :: (denoms fract.numer)
-- TODO implement this and other recursives using List.fold
  

maybeFraction : Overflow -> Maybe NestedFraction
maybeFraction overflow =
  case overflow of 
    Zero ->
      Nothing
    Fraction fract ->
      Just fract


fractionWithDefault : NestedFraction -> Overflow -> NestedFraction
fractionWithDefault default =   
  Maybe.withDefault default << maybeFraction


fractionElse0Over1 : Overflow -> NestedFraction
fractionElse0Over1 =
  fractionWithDefault mempty


childFraction : NestedFraction -> Maybe NestedFraction
childFraction nf =
  nf.numer.overflow |> maybeFraction


compress : MixedNumber -> NestedFraction
compress mn = -- TODO
  fractionElse0Over1 mn.overflow -- delete this


isReduced : NestedFraction -> Bool
isReduced nf =
  let rootIsReduced = 
        nf.numer.wholes < nf.denom 
      childIsReduced =
        Maybe.withDefault True 
          (Maybe.map isReduced <| childFraction nf)
  in 
    rootIsReduced && childIsReduced



-- TODO move this into Factorization module

type alias Factorization = 
  List Int


product : Factorization -> Int 
product = List.product
