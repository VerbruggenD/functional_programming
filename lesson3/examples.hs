{-
    geef de nulpunten van een 2de graadsvergelijking
-}

nulpunten a b c 
    | discriminant < 0 = (-1/0, 1/0)
    | discriminant == 0 = (-b/(2*a), -b/(2*a))
    | otherwise = ((-b-wortel discriminant)/(2*a),(-b+wortel discriminant)/(2*a))
        where 
            wortel x = sqrt(x)
            discriminant = b*b - 4*a*c

{-
    geef het kleinst, grootst en gemiddelde element van een lijst door maar 1x erdoor te gaan
-}

minmaxmean [] = (0,0,0)
minmaxmean (x:xs) = 
    let f [] mi ma som n = (mi, ma, som/n)
        f (x:xs) mi ma som n = f xs (min x mi) (max x ma) (x+som) (n+1)
    in f xs (1/0) (-1/0) 0 0

isDeler kandidaatDeler n = mod n kandidaatDeler == 0
isPriem n = [ deler | deler <- [1..n], isDeler deler n ] == [1,n]
isPriem2 n = [ deler | deler <- [1..(div n 2)], isDeler deler n ] == [1]

qsort [] = [ ]
qsort (x:xs) = kleiner ++ (x:groter)
    where 
        kleiner = qsort [y | y <- xs, y < x]
        groter = qsort [y | y <- xs, y > x]


ietsRaar x y =
    let f y = x * y
    in f y

maakFunctie x =
    let f y = x * y
    in f


m3 = maakFunctie 3

maal2 x = 2 * x
plus5 x = 5 + x

grootsteFunctieResultaat f g x = max (f x) (g x)

grootsteFunctie f g x = if (f x) > (g x) then f
                                            else g

grMaal2Plus5 = grootsteFunctie maal2 plus5 10

{-
    first class citicen
-}

verdubbel = \x -> x * 2
absoluteOptelling = \x y -> abs(x) + abs(y)
absoluteOptelling2 x y = abs(y) + abs(x)

maakVermenig x = \y -> x*y
maal3 = maakVermenig 3
maal10 = maakVermenig 10

maakRechthDrieh z1 z2 = (z1, z2, sqrt(z1*z1 + z2*z2))
maakRechthDrieh20 = maakRechthDrieh 20

{-
    commandos met die lamdas
    map maakRechthDrieh20 [10, 20, 30]
    filter odd [1,2,3,4,5,6]
    filter (\uitslag -> snd uitslag >= 10) [("yoerie", 9), ("iemand anders", 12)]
-}