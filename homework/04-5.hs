
-- newton's method for sqrt

type Tolerance = Double

newton :: Tolerance -> Double -> Double
newton tolerance value = 
	if value < 0 || tolerance < 0
			then error "Both value and tolerance must be positive"
			else newton' tolerance value value 
	where newton' t x y =
						if (abs (newtonNext - y)) < t
								then newtonNext
								else newton' t x newtonNext
						where newtonNext = (y + (x/y)) / 2

-- derivative
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx
    where dx = 0.0001

