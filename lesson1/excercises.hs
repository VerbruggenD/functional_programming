import Debug.Trace

{- halveer de lijst (remove even elementen) -}

evens [] = []
evens (_:xs) = odds xs

odds [] = []
odds (x:xs) = x : evens xs

halveer [] = []
halveer [x] = [x]
halveer (x:y:ys) = x:(halveer ys)


{- verdubbel elk element van de lijst -}

verdubbel [] = []
verdubbel (x:xs) = x:x: (verdubbel xs)

{- ontdubbel de dubbele elementen van de lijst -}

isIn x [] = False
isIn x (y:ys) = if x == y then True
                    else isIn x ys


ontdubbel [] = []
ontdubbel (x:xs)
    | isIn x xs = (ontdubbel xs)
    | otherwise = x:(ontdubbel xs)

{- hermitische som -}

hermitisch 1 = 1
hermitisch n = (1/n) + hermitisch(n-1)

{- vermoeden van fermat
dus is een reeks als het even is gedeeld door 2, 
bij oneven gaat ge *3 en +1 doen en verder werken -}

restMod2 x = mod x 2

fermat 1 = [1]
fermat x
    | even x = x:fermat(div x 2)
    | otherwise = x:fermat((x*3)+1)

{- is priem of geef delers -}

geefDelers n = [x | x <- [1..(n-1)], n `rem` x == 0]

divisors n
  | n < 1 = []
  | otherwise = filter ((== 0) . (mod n)) [1..n]