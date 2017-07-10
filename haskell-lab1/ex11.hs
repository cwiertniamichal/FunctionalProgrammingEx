
rootsIndent :: (Double, Double, Double) -> (Double, Double)
rootsIndent (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where {d = sqrt (b * b - 4 * a * c);
         e = 2 * a} 