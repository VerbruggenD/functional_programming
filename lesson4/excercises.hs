{-
    gewogen gemiddelde van exeman resultaten met het aantal studiepunten
-}

-- telAllesOpVanaf :: Integer -> [Integer] -> Integer

telVakOpBijPunt som (punt, aantalStp) = nieuweSom
    where nieuweSom = som + (punt * aantalStp)

telVakOpBijPuntR (punt, aantalStp) som = nieuweSom
    where nieuweSom = som + (punt * aantalStp)


gewogenGemiddelde examenResultaten = 
  let punten = gewogenPunten examenResultaten
      studiepakket = sum (map snd examenResultaten)
      studiepakket' = foldr (+) 0 (map snd examenResultaten)
  in punten / studiepakket * 5
  
gewogenPunten vakken = foldl telVakOpBijPunt 0 vakken



{-
    kleine oefeningen
-}

-- bereken knopen van een boom

data Tree a = Leaf a
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq, Ord)

voorbeeldBoom = Branch 3 (Leaf 4) (Branch 5 (Leaf 6) (Leaf 7))

{-

boomKnopen (Leaf _) = 1
boomKnopen (Branch _ linkertak rechtertak) = 1 + links + rechts
    where 
        links = boomKnopen linkertak
        rechts = boomKnopen rechtertak

bevatElem x (Leaf y) = x == y
bevatElem x (Branch y links rechts)
    | x == y
    | bevatElem x links = True
    | otherwise = bevatElem x rechts

-}