module NestedFraction where

type NFraction = Whole Int
               | Nested Int NFraction Int

{-| Create an NFraction given a numerator and a 
list of denominators.
-}
nestDiv : List Int -> Int -> NFraction
nestDiv denoms n = 
  case denoms of
    [] -> 
      Whole n 
    (d::ds) -> 
      let totalD = List.product denoms
          wholes = n // totalD
          remain = n %  totalD
      in 
        Whole w `add` Nested 0 (nestDiv ds r) d

{-| Addition operation between two NFractions.

-}
add : NFraction -> NFraction -> NFraction
add nf nf' = 
  case (nf, nf') of
    (Whole w, Whole w') -> 
      Whole (w + w')

    (Nested w n d, Whole w' -> 
      Nested (w + w') n d
    
    (Whole w, Nested w' n' d' ->
      Nested (w + w') n' d'

    (Nested w n d, Nested w' n' d' ->
      let numer = 
        add <|
          n  `mult` (Whole d)  
          n' `mult` (Whole d')
      in 
        Nested (w + w') numer (d * d')


mult : NFraction -> NFraction -> NFraction
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


{-| Return just the `whole` part of an NFraction.
-}
floor : NFraction -> Int 
floor nf = 
  case nf of
    Whole w -> 
      w 
    Nested w _ _ -> 
      w


{-| Return just the fractional part of an NFraction
-}
rem : NFraction -> NFraction
rem nf = 
  case nf of
    Whole _ ->
      Whole 0
    Nested _ n d -> 
      Nested 0 n d


{-| Return a NFraction that equals exactly 0 but contains the same 
hierarchy of denominators as nf.
The numerators of resulting NFraction will all be 0.
-}
zero : NFraction -> NFraction
zero nf = 
  case nf of
    Whole _ ->
      Whole 0
    Nested w n d -> 
      Nested 0 (zero n) d


{-| Return a NFraction that equals exactly 1, but contains
the same hierarchy of denominators as nf.
The numerators of the resulting NFraction will all be equal to
their correspoding denominators.
-}
one : NFraction -> NFraction
one nf = 
  case nf of 
    Whole _ -> 
      Whole 0
    Nested _ n d -> 
      Nested 0 (Whole d `add` one n) d


tick : NFraction -> NFraction
tick nf = nf -- TODO


flatDenom : NFraction -> Int
flatDenom = 
  List.product << denoms
  -- nfFold ((*) << getDenom) 1 nf


denoms : NFraction -> List Int
denoms = 
  List.map snd << toList


toList : NFraction -> List (Int, Int)
toList nf = 
  case nf of
    Whole w -> (w,1) :: []
    Nested w n d -> (w,d) :: toList n


fromList : List (Int,Int) -> NFraction
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
