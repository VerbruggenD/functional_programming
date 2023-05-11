import Data.Time.Format.ISO8601 (yearFormat)
import GHC.Base (VecElem(Int16ElemRep))


data Tree a = Leaf a
    | Branch (Tree a) (Tree a)
        deriving (Eq, Show, Ord)

treeMap f (Leaf a) = Leaf (f a)

treeMap f  (Branch links rechts) = Branch (treeMap f links) (treeMap f rechts)


-- f x y = x + 2 * yearFormat

fmap1 f g = f . g
fmap2 f g = \x -> (f (g x))
fmap3 f g x = f (g x)
fmap4 f g x = (f . g) x
fmap5 f g = (f . g)

main = do
    regel <- getLine
    putStrLn regel
    putStrLn regel

main2 = do
    putStrLn "geef een getal"
    getalTekst <- getLine
    let
        getal = (read getalTekst) :: Int
    putStrLn "geef tekst"
    tekst <- getLine
    let veelTekst = (take getal (repeat tekst)) :: [String]
    let veelPrint = (map putStrLn veelTekst) :: [IO ()]
    putStrLn "------------------"
    sequence_ veelPrint