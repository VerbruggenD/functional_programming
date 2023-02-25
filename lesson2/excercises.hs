import Debug.Trace


aantalDelers n = probeerDelersVanaf 1 n
probeerDelersVanaf kandidaatDeler n
    | kandidaatDeler == n = 1
    | otherwise =
        if mod n kandidaatDeler == 0 then 1 + probeerDelersVanaf (kandidaatDeler + 1) n
                                        else probeerDelersVanaf (kandidaatDeler + 1) n

aantalDelers2 n = probeerDelers [1..n] n
probeerDelers [] n = 0
probeerDelers (x:xs) n = if mod n x == 0 then 1 + probeerDelers xs n
                                            else probeerDelers xs n

{- werkt niet, fout in denkwijze
aantalDelers3 n = 1 + doeDeling 2 n
doeDeling _ 1 = 1
doeDeling kandidaatDeler n
    | mod n kandidaatDeler == 0 = 1 + doeDeling kandidaatDeler (div n kandidaatDeler)
    | otherwise = doeDeling (kandidaatDeler + 1) n 
-}

aantalDelers4 n = length (alleDelers n)
alleDelers n = lijstDelers [1..n] n
lijstDelers [] n = []
lijstDelers (x:xs) n = if mod n x == 0 then x:(lijstDelers xs n)
                                        else lijstDelers xs n

isPriem n = (aantalDelers n) == 2

isPriem2 n = (alleDelers n) == [1,n]

afzonderlijkeCijfers n = if n < 10 then [n]
                                    else (afzonderlijkeCijfers (div n 10))++[mod n 10]

afzonderlijkeCijfers2 n = reverse (afzonderlijkeCijfers3 n)
afzonderlijkeCijfers3 n
    = if n < 10 then [n]
                    else (mod n 10):(afzonderlijkeCijfers3 (div n 10))

uniekeCijfers n 
    | n < 10 = [n]
    | elem (mod n 10) (uniekeCijfers (div n 10)) = (uniekeCijfers (div n 10))
    | otherwise = (mod n 10):(uniekeCijfers (div n 10))


uniekeCijfers2 n
    | n < 10 = [n]
    | elem (laatsteCijfer) (rest) = (rest)
    | otherwise = (laatsteCijfer):(rest)
    where laatsteCijfer = mod n 10
          andereCijfers = div n 10
          rest = uniekeCijfers2 andereCijfers


uniekeCijfers3 n =
    let laatsteCijfer3 = (mod n 10)
        andereCijfers3 = (div n 10)
        rest3 = (uniekeCijfers3 andereCijfers3)
    in if n < 10 then [n]
                    else if elem laatsteCijfer3 rest3 then rest3
                                                        else (laatsteCijfer3):(rest3)

alleDelers2 n = delers n 1 []
    where delers n kandidaatDeler lijst 
            | n == kandidaatDeler = reverse(n:lijst)
            | mod n kandidaatDeler == 0 = delers n (kandidaatDeler + 1)
                                                    (kandidaatDeler:lijst)
            | otherwise = delers n (kandidaatDeler+1) lijst


{- zoeken van langst stijgende deelrij in een lijst -}

langstStijgendeDeelrij [x]= [x]
langstStijgendeDeelrij lijst = reverse (lsd lijst [] [])
    where langst lijst1 lijst2 
            = if (length lijst1) > (length lijst2) then lijst1
                                                    else lijst2
          lsd [] beste huidige = langst beste huidige
          lsd [x,y] beste huidige
            = if x < y then langst beste (x:y:huidige)
                        else langst beste (x:huidige)
          lsd (x:y:xs) beste huidige 
            = if x < y then lsd (y:xs) beste (x:huidige)
                        else lsd (y:xs) (langst beste (x:huidige)) []

langstStijgendeDeelrij2 [] = []
langstStijgendeDeelrij2 (x:xs) = reverse (lsd xs [] [x] x)
    where langst lijst1 lijst2
                = if (length lijst1) > (length lijst2) then lijst1
                                                else lijst2
          lsd [] beste huidige vorige = langst beste huidige
          lsd (x:xs) beste huidige vorige
            = if vorige < x then lsd xs beste (x:huidige) x
                            else lsd xs (langst beste huidige) [x] x

{- oefening gelukkig getal 
    - kwadrateer de afzonderlijke cijfers
    - neem som van de kwadraten
    - herhaal tot 1 bekomt of er getal uitkomt dat al is is voorgekomen (dus een cyclus gedetecteerd)
    - in het geval van uitkomst van 1 dan is het een gelukkig getal
-}


gelukkigGetal n = gelukkig n []
    where gelukkig 1 _ =  True
          gelukkig n vorige 
                = let cijfers = afzonderlijkeCijfers2 n 
                      kwadratenSom = kwadraten cijfers
                  in if elem kwadratenSom vorige then False
                                                  else gelukkig kwadratenSom (n:vorige)
          kwadraten [] = 0
          kwadraten (x:xs) = x * x + kwadraten xs


{-
geef min max en gemiddelde van een lijst met maar 1 passage
-}

maxLijst [x] = x
maxLijst (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxLijst xs

avgLijst :: [Double] -> Double
avgLijst [] = 0
avgLijst (x:xs) = avg (x:xs) (fromIntegral (length (x:xs)))
    where
        avg :: [Double] -> Double -> Double
        avg [] _ = 0
        avg (x:xs) l =  (x / l) + (avg xs l)

analyseerLijst (x:xs) = (minVal (x:xs), maxVal (x:xs), avgVal (x:xs))
    where
        minVal [x] = x
        minVal (x:xs)
            | x < minTail = x
            | otherwise = minTail
            where minTail = minVal xs
        maxVal [x] = x
        maxVal (x:xs)
            | x > maxTail = x
            | otherwise = maxTail
            where maxTail = maxVal xs
        avgVal :: [Double] -> Double
        avgVal [] = 0
        avgVal (x:xs) = avg (x:xs) (fromIntegral (length (x:xs)))
            where
                avg :: [Double] -> Double -> Double
                avg [] _ = 0
                avg (x:xs) l =  (x / l) + (avg xs l)

analyseerLijst2 [x] = (x, x, x)
analyseerLijst2 (x:xs) = (minimum (x:xs), maximum (x:xs), (avg (x:xs) (fromIntegral (length (x:xs)))))
    where
        avg :: [Double] -> Double -> Double
        avg [] _ = 0
        avg (x:xs) l =  (x / l) + (avg xs l)

{- 
zoek de nulpunten van de 2de graads functie van de vorm axˆ2 + bx + c
-}
nulpunten a b c = trace (show d) (root1, root2)
    where
        d = (b * b) - 4 * a * c
        root1 = ((((-1) * b) + sqrt (d)) / (2*a))
        root2 = ((((-1) * b) - sqrt (d)) / (2*a))