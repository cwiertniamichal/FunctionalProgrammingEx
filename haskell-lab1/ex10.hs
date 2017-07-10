rootsLet :: (Double, Double, Double) -> (Double, Double)
rootsLet (a, b, c) = 
    let {d = sqrt (b * b - 4 * a * c);
        e = 2 * a
    }
    in ( (-b - d) / e, (-b + d) / e) 


unitVec2DLet :: (Double, Double) -> (Double, Double)
unitVec2DLet (x, y) =
    let unit = sqrt(x^2 + y^2) 
    in (x / unit, y / unit)

unitVec3DLet :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3DLet (x, y, z) = 
    let unit = sqrt(x^2 + y^2 + z^2)
    in (x / unit, y / unit, z / unit)

triangleAreaLet :: (Double, Double, Double) -> Double
triangleAreaLet (a, b, c) = 
    let p = 0.5 * (a + b + c)
    in sqrt(p * (p - a) * (p - b) * (p - c))