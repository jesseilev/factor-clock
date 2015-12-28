module NestedFraction where

type NestedFraction 
  = Whole Int
  | Nested Int NestedFraction Int

{-| Create an NestedFraction given a numerator and a 
list of denominators.
-}
nestDiv : List Int -> Int -> NestedFraction
nestDiv denoms n = 
  case denoms of
    [] -> 
      Whole n 
    (d::ds) -> 
      let totalD = List.product denoms
          wholes = n // totalD
          remain = n %  totalD
      in 
        Whole wholes `add` Nested 0 (nestDiv ds remain) d

{-| Addition operation between two NestedFractions.

-}
add : NestedFraction -> NestedFraction -> NestedFraction
add nf nf' = 
  case (nf, nf') of
    (Whole w, Whole w') -> 
      Whole (w + w')

    (Nested w n d, Whole w') -> 
      Nested (w + w') n d
    
    (Whole w, Nested w' n' d') ->
      Nested (w + w') n' d'

    (Nested w n d, Nested w' n' d') ->
      let numer = 
        (n `mult` (Whole d))
        `add`
        (n' `mult` (Whole d'))
      in 
        Nested (w + w') numer (d * d')


mult : NestedFraction -> NestedFraction -> NestedFraction
mult nf nf' =
  case (nf, nf') of
    (Whole w, Whole w') -> 
      Whole (w * w')

    (Nested w n d, Whole w') -> 
      Nested (w * w') n d

    (Whole w, Nested w' n' d') -> 
      Nested (w * w') n' d'

    (Nested w n d, Nested w' n' d') -> 
      Nested (w * w') (n `mult` n') (d * d')


{-| Return just the `whole` part of an NestedFraction.
-}
floor : NestedFraction -> Int 
floor nf = 
  case nf of
    Whole w -> 
      w 
    Nested w _ _ -> 
      w


{-| Return just the fractional part of an NestedFraction
-}
rem : NestedFraction -> NestedFraction
rem nf = 
  case nf of
    Whole _ ->
      Whole 0
    Nested _ n d -> 
      Nested 0 n d


{-| Return a NestedFraction that equals exactly 0 but contains the same 
hierarchy of denominators as nf.
The numerators of resulting NestedFraction will all be 0.
-}
zero : NestedFraction -> NestedFraction
zero nf = 
  case nf of
    Whole _ ->
      Whole 0
    Nested w n d -> 
      Nested 0 (zero n) d


{-| Return a NestedFraction that equals exactly 1, but contains
the same hierarchy of denominators as nf.
The numerators of the resulting NestedFraction will all be equal to
their correspoding denominators.
-}
one : NestedFraction -> NestedFraction
one nf = 
  case nf of 
    Whole _ -> 
      Whole 0
    Nested _ n d -> 
      Nested 0 (Whole d `add` one n) d


tick : NestedFraction -> NestedFraction
tick nf = nf -- TODO


flatDenom : NestedFraction -> Int
flatDenom = 
  List.product << denoms
  -- nfFold ((*) << getDenom) 1 nf


denoms : NestedFraction -> List Int
denoms = 
  List.map snd << toList


toList : NestedFraction -> List (Int, Int)
toList nf = 
  case nf of
    Whole w -> (w,1) :: []
    Nested w n d -> (w,d) :: toList n


fromList : List (Int,Int) -> NestedFraction
fromList l = 
  case l of 
    [] -> Whole 0
    ((w,d) :: xs) ->
      Nested w (fromList xs) d



-- TODO Consider refactoring to this model:

type NestFract
  = NestFract 
      { wholes : Int
      , fract : Maybe Fract
      }

type alias Fract =  
  { numer : NestFract
  , denom : Int
  }
