isIn x [] = False
isIn x (y:ys)
    | x == y = True
    | otherwise = isIn x ys

bevatA woord = isIn 'a' woord
bevatA' = isIn 'a'

{- filter (isIn 's') [woord1, woord2, woord3] dit is een vorm van currying -}

-- types

data Geslacht = M | V | X deriving (Show, Eq)

type Naam = String
type Leeftijd = Integer

data Mens = Mens {
                    naam :: Naam,
                    leeftijd :: Leeftijd,
                    geslacht :: Geslacht
                } deriving (Show, Eq)

jan = Mens "Jan" 25 M
kris = Mens "Kris" 52 M

gemiddeldeLeeftijd mensen = (fromIntegral somLeeftijd) / (fromIntegral (length mensen))
    where somLeeftijd =sum ( map leeftijd mensen)


data Point a = Pt a a

afstand (Pt x1 y1) (Pt x2 y2) = sqrt (dx * dx + dy * dy)
        where  dx = x2- x1
               dy = y2 - y1 


data Expr = Cte Integer 
            | Add Expr Expr 
            | Sub Expr Expr
            | Mult Expr Expr

eval (Cte n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)


data Tree a = Leaf a
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq, Ord)

voorbeeldBoom = Branch 3 (Leaf 4) (Branch 5 (Leaf 6) (Leaf 7))
tekstBoom = Branch "fsfg" (Leaf "sfhdjl") (Branch "sigdhfjf" (Leaf "sdwuhd") (Leaf "whodh"))

somBoom (Leaf getal) = getal
somBoom (Branch getal linkertak rechtertak) =
    getal + (somBoom linkertak) + (somBoom rechtertak)

diepteBoom (Leaf _) = 0
diepteBoom (Branch _ linkertak rechtertak) = 1 + max diepteLinks diepteRechts
    where 
        diepteLinks = diepteBoom linkertak
        diepteRechts = diepteBoom rechtertak