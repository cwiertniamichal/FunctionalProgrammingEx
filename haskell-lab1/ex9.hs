roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ((-b - d) / e, (-b + d) / e)
    where d = sqrt (b * b - 4 * a * c)
          e = 2 * a


unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x / unit, y / unit)
    where unit = sqrt(x^2 + y^2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x / unit, y / unit, z / unit)
    where unit = sqrt(x^2 + y^2 + z^2)

triangleAreaWhere :: (Double, Double, Double) -> Double
triangleAreaWhere (a, b, c) = sqrt(p * (p - a) * (p - b) * (p - c))
    where p = 0.5 * (a + b + c)