module Circle where


import Graphics.Collage as Clg


radius : Float
radius = 1


{-| The affine transform for circle-packing a child circle
inside a larger parent circle.
  -- TODO move to PieChart or related module
-}
circlePackTransform : Int -> Int -> Clg.Form -> Clg.Form
circlePackTransform total numer =
  let bigAng = turns (1 / toFloat total)
      lilAng = bigAng / 2
      st     = sin (lilAng)
      lilRad = radius * st / (st + 1)
      dist   = radius - lilRad
      scale  = lilRad / radius
      rot    = toFloat numer * bigAng
      move   = fromPolar (dist, rot + lilAng)
      extraRot = lilAng + (turns 0.5)
  in  
  -- TODO factor out scale, which is numerator agnositc
    Clg.move move 
      << Clg.rotate (rot + extraRot) 
      << Clg.scale (scale * 0.85)
    -- TODO this 0.85 padding doesn't belong here, factor it out
    -- to some style config
