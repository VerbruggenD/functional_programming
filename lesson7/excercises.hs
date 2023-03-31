import Debug.Trace
{- zoek de waarde die het meeste voorkomt in een lijst:
- qsort de lijst
- variabele voor het tellen van huidige, huidige beste waarde, huidige beste element
- door de lijst gaan en de elementen tellen die hetzelfde zijn   
-}

qsort [] = []
qsort (x:xs) = kleiner ++ (x:groter)
    where 
        kleiner = qsort [y | y <- xs, y < x] 
        --
        groter = qsort [y | y <- xs, y >= x]
        
frequencyInList [] = 0
frequencyInList xs = frequency (qsort xs) 0 0 0 0
    where 
        frequency [] huidigeFreq vorigElement besteElem besteFreq = besteElem
        frequency (x:xs) huidigeFreq vorigElement besteElem besteFreq 
             = 
                (if (x == vorigElement) then
                    if huidigeFreq > besteFreq then
                        let besteElem = x
                            besteFreq = huidigeFreq
                        in  frequency xs (huidigeFreq + 1) x besteElem besteFreq
                    else frequency xs (huidigeFreq + 1) x besteElem besteFreq
                 else frequency xs 1 x besteElem besteFreq)


{-
    hermitische som met staart recursie
-}

harmonicTail :: Double -> Double
harmonicTail n = helper 1 0
    where helper i acc
            | i > n     = acc
            | otherwise = helper (i + 1) (acc + 1 / i)

{- snelle machtsfunctie -}

power :: Int -> Int -> Int
power n p = power' n p 1
  where
    power' n 0 acc = acc
    power' n p acc = power' n (p-1) (n*acc)

{-
    knopen tellen van een boom
-}

-- data Tree a = Leaf a | Branch (Tree a) (Tree a)

countNodes :: Tree a -> Int
countNodes (Leaf _) = 1
countNodes (Branch l r) = countNodes l + countNodes r + 1

t = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

{- oefeningen les 6
    is dit een palindrome?
-}

isPalindrome :: String -> Bool
isPalindrome s = isPalindrome' s []
  where
    isPalindrome' [] acc = s == acc
    isPalindrome' (x:xs) acc = isPalindrome' xs (x:acc)

{- oefeningen op type tree -}

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Ord, Show)

left :: Tree a -> Tree a
left (Branch l _) = l

right :: Tree a -> Tree a
right (Branch _ r) = r

diepteBoom t = diepteBoom' t 0
    where
        diepteBoom' (Leaf _) acc = acc + 1
        diepteBoom' (Branch linkertak rechtertak) acc = max (diepteBoom' linkertak (acc+1)) (diepteBoom' rechtertak (acc+1))


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch left right) = Branch (mapTree f left) (mapTree f right)

