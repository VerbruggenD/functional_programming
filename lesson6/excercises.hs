import Data.Maybe

{- begining from the file on Toledo -}

class Overlappend a where
    isOverlappend :: a -> a -> Bool
    unie :: a -> a -> a

-- class x extends y
-- interface x extends y

class Overlappend a => Splitsbaar a where
    doorsnede :: a -> a -> Maybe a -- niet als doorsnede niet bestaad

{-- rechthoek --}   
data Rechthoek = Rechthoek { links :: Int,
                             rechts :: Int,
                             onder :: Int,
                             boven :: Int}
    deriving (Eq, Ord, Show)
    -- oorsprong assenstelstel linksboven

instance Overlappend Rechthoek where
    isOverlappend rh1 rh2 = (overlaptH1 || overlaptH2)
                        &&
                       (overlaptV1 || overlaptV2)
        where l1 = links rh1
              l2 = links rh2
              r1 = rechts rh1
              r2 = rechts rh2
              o1 = onder rh1
              o2 = onder rh2
              b1 = boven rh1
              b2 = boven rh2
              overlaptH1 = l2 <= r1 && r2 >= l1
              overlaptH2 = l1 <= r2 && r1 >= l2
              overlaptV1 = o2 <= b1 && b2 >= o1
              overlaptV2 = o1 <= b2 && b1 >= o2
    unie rh1 rh2 = Rechthoek { links = min (links rh1) (links rh2),
                               rechts = max (rechts rh1) (rechts rh2),
                               boven = max (boven rh1) (boven rh2),
                               onder = min (onder rh1) (onder rh2) }

instance Splitsbaar Rechthoek where
    doorsnede rh1 rh2 = if isOverlappend rh1 rh2 then Just kleineRechthoek
                                                 else Nothing
        where kleineRechthoek = Rechthoek { links = max (links rh1) (links rh2),
                                            rechts = min (rechts rh1) (rechts rh2), 
                                            boven = min (boven rh1) (boven rh2),
                                            onder = max (onder rh1) (onder rh2) }
                                            
maakRechthoek :: Int -> Int -> Int -> Int -> Rechthoek              
maakRechthoek x1 x2 y1 y2 = Rechthoek (min x1 x2) (max x1 x2) (max y1 y2) (min y1 y2)
             
rh1 = Rechthoek 4 10 2 5
rh2 = Rechthoek 2 8 (-2) 3 
rh3 = Rechthoek 5 8 (-2) 3
rh4 = Rechthoek 5 14 (-2) 3
rh5 = Rechthoek 2 14 (-2) 3
rh6 = Rechthoek 1 3 3 4
rh7 = Rechthoek 14 15 1 8
   
rhs = [rh1, rh2, rh3, rh4, rh5, rh6, rh7]

{- 
    OEFENING: implementeerbaar Overlappen en Splitsbaar voor Int  
        - twee getallen overlappen wanneer ze gemeenschappelijke delers hebben groter dan 1
        - unie = product van alle priemdelers, bv. unie 8 (2*2*2*) en 64 (2*2*2*2) = 64 (2*2*2*2), 
                                                   unie 24 (2*2*2*3) en 20 (2*2*3*5) = 120 (2*2*2*3*5)
                     grootste gemene deler -> standaard functie in Haskell  
        - doorsnede = product van de gemeenschappelijke priemdelers,
                            bv. doorsnede (8::Int) (2*2*2*) en (64::Int) (2*2*2*2) = 8 (2*2*2), 
                                doorsnede (24::Int) (2*2*2*3) en (60::Int) (2*2*3*5) = 12 (2*2*3)        
-}
delers n = [x | x <- [1..n], mod n x == 0]   

priemDelers n = (priemDelers' 2 n)
  where priemDelers' 1 1 = [1]
        priemDelers' kandidaatDeler m | kandidaatDeler > m        = [] 
                                      | kandidaatDeler == m       = [kandidaatDeler]
        priemDelers' kandidaatDeler n | mod n kandidaatDeler == 0 = kandidaatDeler:(priemDelers' kandidaatDeler (div n kandidaatDeler))
                                      | otherwise                 = priemDelers' (kandidaatDeler+1) n

gemeenschappelijk [] _ = []
gemeenschappelijk _ [] = []
gemeenschappelijk (x:xs) (y:ys) | x == y    = x:(gemeenschappelijk xs ys)
                                | x < y     = gemeenschappelijk xs (y:ys)
                                | otherwise = gemeenschappelijk (x:xs) ys    
                                
union [] xs = xs
union xs [] = xs
union (x:xs) (y:ys) | x == y    = x:(union xs ys)
                    | x < y     = x:(union xs (y:ys))
                    | otherwise = y:(union (x:xs) ys)    

instance Overlappend Int where
   -- twee getallen overlappen wanneer ze gemeenschappelijke delers hebben groter dan 1
   isOverlappend a b = length(gemeenschappelijk (delers a) (delers b))>1    -- bij gebruik (a::Int) specifieren
   unie a b = product(union (priemDelers a) (priemDelers b))

     
-- instance Splitsbaar Int where
instance Splitsbaar Int where
    doorsnede a b = if isOverlappend a b then Just (product(gemeenschappelijk (priemDelers a) (priemDelers b)))
                                         else Nothing

-- EXTRA OEFENING VOOR DE SNELLE STUDENT EN/OF THUIS -- 

--data Leeftijdsgroep = Groep Int Int
data Leeftijdsgroep a = Groep a a

-- instance Ord a => Overlappend (Leeftijdsgroep a) where

    
{------------------------------------
 -  IMPLEMENTATIE VAN DE ALGORITMES -
 ------------------------------------}

-- niet nodig om te declareren, maar wordt hier als tip meegegeven
-- overlappendeUnie :: Overlappend a => [a] -> [a]
-- overlappendeUnie [] = Nothing
-- overlappendeUnie

eenStap (x:xs) = nieuweLijst
    where
        overlappendeElem = filter (isOverlappend x) xs
        overlapper = foldl unie x overlappendeElem
        nietOverlappendeElem = filter (\y -> not (isOverlappend x y)) xs
        nietOverlappendeElemX y = not (isOverlappend x y)
        nietOverlappendeElem' = filter nietOverlappendeElemX xs
        nieuweLijst = overlapper:nietOverlappendeElem

eersteLus [] = []
eersteLus (x:xs) = if length overlappendeElem == 0 then x: (eersteLus xs)
                                                        else eersteLus stap1
    where 
        overlappendeElem = filter (isOverlappend x) xs
        stap1 = eenStap (x:xs)

overlappendeUnie xs = if nieuweLijst == xs then nieuweLijst                     -- vast punt
                                            else overlappendeUnie nieuweLijst
    where nieuweLijst = eersteLus xs

-- recursieveDoorsnede :: Splitsbaar a => [a] -> Maybe a
recursieveDoorsnede [] = Nothing
recursieveDoorsnede (x:xs) = doorsnede' (Just x) xs
    where 
        doorsnede' :: Splitsbaar a => Maybe a -> [a] -> Maybe a
        doorsnede' a [] = a
        doorsnede' a (x:xs) = if isOverlappend (fromJust a) x then doorsnede' (doorsnede (fromJust a) x) xs
                                                    else Nothing

rd [] = Nothing
rd [x] = Just x
rd (x:y:ys) = if isOverlappend x y then rd ((fromJust (doorsnede x y)):ys)
                                    else Nothing

{- nieuwe opdracht: wat is de afstand tussen 2 strings? -}

data Edit = Copy Char
            | Insert Char
            | Replace Char Char
            | Delete Char
            | DeleteRest String
                deriving Show

transformeer :: String -> String -> [ [Edit] ]
transformeer [] xs = [map Insert xs]
transformeer xs [] = [ [DeleteRest xs] ]
transformeer (x:xs) (y:ys)
    | x == y    = map (\edits -> (Copy x):edits) (transformeer xs ys)
    | otherwise = optie1 ++ optie2 ++ optie3
    where
        optie1 = map (\edits -> (Insert y):edits) (transformeer (x:xs) ys)
        optie2 = map (\edits -> (Delete x):edits) (transformeer xs (y:ys))
        optie3 = map (\edits -> (Replace x y):edits) (transformeer xs ys)

kostprijs [] = 0
kostprijs ((Copy _):edits) = kostprijs edits
kostprijs (_:edits) = 1 + kostprijs edits

goedkoopsteTransformatie ts =
    let prijzenTS = map (\edits -> (kostprijs edits, edits)) ts
        eersteDuo = head prijzenTS
        andereDuos = tail prijzenTS
        goedkoopste (prijs1, edits1) (prijs2, edits2) = if prijs1 < prijs2 then (prijs1, edits1)
                                                                            else (prijs2, edits2)
        goedkoopsteDuo = foldr goedkoopste eersteDuo andereDuos
    in goedkoopsteDuo

afstandsString string1 string2 = fst (goedkoopsteTransformatie (transformeer string1 string2))