
somOud [] = 0
somOud (x:xs) = x + somOud xs

som xs = somTail 0 xs
somTail n [] = n
somTail !n (x:xs) = somTail (n+x) xs
-- het uitroepteken is om aan te geven aan de compiler dat dit getal ook ineens moet 
--- uitgerekend worden want dit is nodig voor de laatste bewerking

fib 1 = 1 
fib 2 = 1 
fib n = fib (n-1) + fib (n-2)
-- 1 1 2 3 5
-- 1 2 3 4 5

fib2 n = fibAcc 2 n 1 1

fibAcc teller n vorige huidige = 
    if teller == n then huidige
                    else fibAcc (teller + 1) n huidige (vorige + huidige)


uniekeCijfersOud getal 
   | getal < 10 = [getal]
   | elem laatsteCijfer volgendeUniekeCijfers = volgendeUniekeCijfers
   | otherwise = laatsteCijfer:volgendeUniekeCijfers
   where laatsteCijfer = mod getal 10
         andereCijfers = div getal 10
         volgendeUniekeCijfers = uniekeCijfersOud andereCijfers

uniekeCijfers getal = uniek [] getal
uniek cijfers getal 
    | getal == 0 = cijfers
    | laatsteCijfer `elem` cijfers = uniek cijfers andereCijfers
    | otherwise = uniek (laatsteCijfer:cijfers) andereCijfers
    where 
        laatsteCijfer = mod getal 10
        andereCijfers = div getal 10

qsort [] = []
qsort (x:xs) = kleiner ++ (x:groter)
    where 
        kleiner = qsort [y|y <- xs, y < x]
        groter = qsort [y|y <- xs, y > x]