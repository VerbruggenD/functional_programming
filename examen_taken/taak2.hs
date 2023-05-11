{-
    Generieke kwaliteitscontrole:
        - op basis van verschillende criteria of er iets moet gebeuren (zoals onderhoud)
        - indeling voor 1 criterium 
                - in orde
                - lichte afwijking
                - grote afwijking
                - niet in orde
        - marges voor criterium volledig dynamisch
        - dan eisen hoeveel criteria kleine of grote afwijking mogen hebben (punten systeem?)
            - punten per indeling instellen
        - ook tov totaal (vb: punten van examens, met maar 1 kleine afwijking moet het totaal minder zijn om te slagen)
        - zowel relatief (procentueel) kunnen werken als absoluut
-}

data Component = Component {
    weigth :: Int,
    okayBound :: Int,
    smallErrorBound :: Int,
    largeErrorBound :: Int,
    qualityDirection :: QualityDirection
} deriving (Show, Eq)

data QualityDirection = Lower | Higher deriving (Show, Eq)

data ComponentStatus = Okay | SmallError | LargeError | Fatal deriving (Eq, Show)

data ProductStatus = Pass | Fail deriving (Eq, Show)

type CheckQuality = Component -> Int -> ComponentStatus

checkComponent :: CheckQuality
checkComponent component value =
    case qualityDirection component of
        Higher -> 
            if value > okayBound component then Okay
                    else if value > smallErrorBound component then SmallError
                        else if value > largeErrorBound component then LargeError
                            else Fatal
        Lower -> 
            if value < okayBound component then Okay
                    else if value < smallErrorBound component then SmallError
                        else if value < largeErrorBound component then LargeError
                            else Fatal

CheckQuality :: [Component] -> ProductStatus
CheckQuality [component] = 

band = Component { weigth = 1, okayBound = 5, smallErrorBound = 10, largeErrorBound = 15, qualityDirection = Lower}
stuur = Component { weigth = 100, okayBound = 500, smallErrorBound = 1000, largeErrorBound = 1500, qualityDirection = Lower}

auto = [band, stuur]