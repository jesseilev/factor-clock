module NumExtra where
-- TODO figure out how to attach this to some Core library  
-- eg List.Extra


divf : Int -> Int -> Float 
divf x y = 
  toFloat x / toFloat y

divRound : Int -> Int -> Int
divRound x y = 
  round << divf x <| y

