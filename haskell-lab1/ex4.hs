sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (n, l) = (l, n)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == y) && (y == z)

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt((a + b + c) * (a + b - c) * (a - b + c) * (-a + b + c)) / 4

foo :: Int -> Int
foo a = a