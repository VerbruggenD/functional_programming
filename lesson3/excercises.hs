import Data.Char

isDeler kandidaatDeler n = mod n kandidaatDeler == 0
isPriem n = [ deler | deler <- [1..n], isDeler deler n ] == [1,n]

{-
    geef alle delers van een getal door list comprehension toe te passen, (geen recursie)
-}

geefDelers n = [x | x <- [1..(n)], rem n x == 0]

geefDelers2 n = [x | x <- [1..(div n 2)], isDeler x n] ++ [n]

{-
    geef een lijst terug met alle elementen die in de 2 gegeven lijsten staan
-}

gelijkeElementen xs ys = [x | x <- xs, y <- ys, x == y]

gelijkeElementen2 xs ys = [x | x <- xs, elem x ys]

{-
    ontbind een gegeven getal in alle mogelijke combinaties met 2 nieuwe getallen, 
    geef deze mogelijkheden in lijst van tuples
-}

ontbindGetal n = [ (x,y) | x <- [1..n], y <- [1..n], x * y == n]

ontbindGetal2 n = [ (x,y) | x <- [1..n], isDeler x n , y <- [div n x]]

ontbindGetal3 n = [ (x,y) | x <- [1..(wortel n)], isDeler x n , y <- [div n x]]
    where
        wortel a = floor (sqrt(a))

{-
    geef de getallen die ontbonden kunnen worden in priemfactoren en de priemfactoren zelf tussen twee gegeven getallen (reeks)
-}

priemfactoren n = [ priemfact | (priemfact, _) <- ontbindGetal n, isPriem priemfact]


driehoeken = [ (zijde1, zijde2, zijde3) | zijde1 <- [1..24],
                                            zijde2 <- [1..24], zijde3 <- [1..24],
                                            zijde1 + zijde2 + zijde3 == 24,
                                            zijde1 * zijde1 + zijde2 * zijde2 == zijde3 * zijde3]


{-
    oefeningen op map en filter
-}

lettersUpper xs = map toUpper xs

isHoofdLetter xs = filter isUpper xs

klinkerHoofdletter xs = map toUpper (filter (\x -> elem x "aeiou") xs)
klinkerInHoofdletter xs = 
     if map (\x -> elem x "aeiou") then (toUpper x)
                                else x