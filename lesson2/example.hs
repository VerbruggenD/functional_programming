
macht x 0 = 1
macht x n | even n = (macht x (div n 2)) ^ 2
            | otherwise = x * (macht x (n-1))

machtSimpel x 0 = 1
machtSimpel x n = x * (machtSimpel x (n-1))

{-
ggd zoeken met euclides
-}

ggd a 0 = a
ggd a b = ggd (min a b) (mod (max a b) (min a b))

{-
ggd 1e oplossing, met delers van beiden zoek en grootste nemen van beiden, dit is veel minder goed als met euclides
-}

