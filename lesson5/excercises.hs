import Data.Char
{-
type class: maak data type voor imaginaire/complex getal en implementeer type class num
-}

data Complex = Complex {
                    reeel :: Double,
                    imaginair :: Double
                } deriving (Show)

-- aan de hand van :i met de Num kijken welke bewerkingen geimplementeerd moeten worden

instance Num Complex where
    (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
    negate (Complex r i) = Complex (-r) (-i)
    (Complex r1 i1) * (Complex r2 i2) = 
        let 
            r = r1 * r2
            i = -(i1 * i2)
            i11 = r1 * i2
            i22 = i1 * r2
        in Complex (r+i) (i11+i22)
    abs (Complex r i) = Complex (sqrt(r*r+i*i)) 0
    signum (Complex r _) = Complex (signum r) 0
    fromInteger n = Complex (fromInteger n) 0

{-
Implementeer listable int zodat die lijst van delers terug geeft
-}

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList n = [x | x <- [1..n], mod n x == 0]

instance Listable [Char] where
    toList woord = map ord woord

{-
oefening op kostprijs
-}

class Kostprijs a where
    getPrijs :: a -> Double

data Bakbier = Jupiler | Stella

instance Kostprijs Bakbier where
    getPrijs Jupiler = 12
    getPrijs Stella = 14

data Brood = Brood Bool

instance Kostprijs Brood where
    getPrijs (Brood True) = 3.00
    getPrijs (Brood False) = 2.00

bakkenBier = [Jupiler, Jupiler, Stella]
broden = [Brood True, Brood True, Brood False]

producten = [Left Jupiler, Left Stella, Right (Brood False)]
-- left en right zijn producten van either

data Banaan = Banaan Double

instance Kostprijs Banaan where
    getPrijs (Banaan gewicht) = gewicht * 1.70

analyseWinkelkarretje producten = (goedkoopste, duurste, totaal)
    where
        prijzen = map getPrijs producten
        goedkoopste = minimum (prijzen)
        duurste = maximum (prijzen)
        totaal = sum prijzen

data Tree a = Leaf a
    | Branch (Tree a) (Tree a)
        deriving (Eq, Show, Ord)

instance Kostprijs (Tree Double) where
    getPrijs (Leaf a) = a
    getPrijs (Branch l r) = 2*(getPrijs l + getPrijs r)

boom1 = Branch (Leaf 4.0) (Branch (Leaf 3.0) (Branch (Leaf 2.0) (Leaf 1.0)))
boom2 = Branch (Branch (Leaf 1.0) (Leaf 2.0)) (Branch (Leaf 3.0) (Leaf 4.0))

{-
overlappende figuren
1. eerst de figuren definieren, cirkel met midden en straal, rechthoek met linker onderhoek en lengte breedte
2. detecteer overlappingen
3. maak omsluitende figuur
4. uitbreiding: bereken oppervlakte, functie om te tesen of een figuur een andere omsluit
-}

-- data Circle x y r = Circle Double Double Double

-- data Rectangle x y l w = Rectangle Double Double Double Double

-- class IsOverlapping a b where
--     checkOverlap :: a b -> Bool

-- instance IsOverlapping Circle Circle where
--     checkOverlap 