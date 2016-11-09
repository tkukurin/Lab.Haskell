
-- newton's method for sqrt

type Tolerance = Double

-- derivative
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx
    where dx = 0.0001

