-- fmap f (Just x) = Just (f x)
-- fmap f Nothing  = Nothing

fmap1 f g = f . g
fmap2 f g = \x -> (f (g x))
fmap3 f g x = f (g x)
fmap4 f g x = (f . g) x
fmap5 f g = (f . g)

-- law 2 of a functor

-- fmap (f . g) Nothing = ((fmap f) . (fmap g)) Nothing -- bewijs

-- fmap (f . g) Nothing = Nothing   -- geeft foutmelding

-- ((fmap f) . (fmap g)) Nothing = (fmap f) (fmap g Nothing) = (fmap f) Nothing = Nothing




-- implementeer fmap voor Either a

data Ofwel a b = Teen a | Tander b

instance Functor (Ofwel a) where
    fmap f (Teen x) = Teen x
    fmap f (Tander x) = Tander (f x)

-- instance Functor (Either a) where
--     fmap f (Left a) = Left a
--     fmap f (Right a) = Right (f a)

{-
    hoger lager

    - je start de functie met een getal (eigenlijk geheim voor de gebruiker)
    - gebruiker geeft getal in
    - programma zegt hoger of lager tot dat het juist is
    - extra: tel het aantal pogingen tot het juist geraden is
-}

hogerLager :: Int -> IO ()
hogerLager n = do
    putStrLn "geef een getal in"
    getalTekst <- getLine
    let getal = (read getalTekst) :: Int
    if getal < n then do
        putStrLn "hoger"
        hogerLager n
                    else if getal > n then do
                        putStrLn "lager"
                        hogerLager n
                        else putStrLn "juist"

hogerLagerPogingen :: Int -> IO Int
hogerLagerPogingen n = 
    let hogerlager n teller = do
         putStrLn "geef een getal in"
         getalTekst <- getLine
         let getal = (read getalTekst) :: Int

         if getal == n then return teller
            else do
                if getal < n then putStrLn "hoger"
                             else putStrLn "lager"
                hogerlager n (teller+1)
                
    in hogerlager n 1

main = hogerLager 27

main2 = do pogingen <- hogerLagerPogingen 27
           if pogingen < 5 then putStrLn ("flink slechts " ++ show pogingen ++ " pogingen")
                            else putStrLn ("loser niet minder dan " ++ show pogingen ++ " pogingen")