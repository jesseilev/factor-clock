module NestedFraction where

type NFraction = Whole Int
               | Nested Int NFraction Int

nestDiv : List Int -> Int -> NFraction
nestDiv dnms n = case dnms of
  []     -> Whole n 
  (d::ds) -> 
    let totalD = List.product dnms
        w      = n // totalD
        r      = n %  totalD
    in Whole w `plus` Nested 0 (nestDiv ds r) d

plus : NFraction -> NFraction -> NFraction
plus nf nf' = case (nf, nf') of
  (Whole w, Whole w')        -> Whole (w + w')
  (Nested w n d, Whole w')   -> Nested (w + w') n d
  (Whole w, Nested w' n' d') -> Nested (w + w') n' d'
  (Nested w n d, Nested w' n' d') ->
    let numer =  timz n  (Whole d) 
                `plus`  
                (timz n' (Whole d'))
    in Nested (w + w') numer (d * d')

timz : NFraction -> NFraction -> NFraction
timz nf nf' = case (nf, nf') of
  (Whole w, Whole w')        -> Whole (w * w')
  (Nested w n d, Whole w')   -> Nested (w * w') n d
  (Whole w, Nested w' n' d') -> Nested (w * w') n' d'
  (Nested w n d, Nested w' n' d') -> 
    Nested (w * w') (n `timz` n') (d * d')

floor : NFraction -> Int 
floor nf = case nf of
  (Whole w)      -> w 
  (Nested w _ _) -> w

rem : NFraction -> NFraction
rem nf = case nf of
  (Whole _)      -> Whole 0
  (Nested _ n d) -> Nested 0 n d

zero : NFraction -> NFraction
zero nf = case nf of
  (Whole _)      -> Whole 0
  (Nested w n d) -> Nested 0 (zero n) d

one : NFraction -> NFraction
one nf = case nf of 
  (Whole _)      -> Whole 0
  (Nested _ n d) -> Nested 0 (Whole d `plus` one n) d

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
