import Data.Maybe
import Debug.Trace


data QualityDirection = Lower | Higher deriving (Show, Eq)

data Status = Pass | SmallError | LargeError | Fail deriving (Eq, Show)

data Component = Component {
    value :: Double,
    weight :: Maybe Int,
    okayThreshold :: Double,
    smallErrorThreshold :: Double,
    largeErrorThreshold :: Maybe Double,
    qualityDirection :: QualityDirection
} deriving (Show, Eq)

data Product = Product {
    components :: [Component],
    errorPoints :: (Int, Int), -- (smallErrorPoints, largeErrorPoints)
    absoluteThreshold :: Int,
    relativeThreshold :: Maybe (Float, Int) -- (percentageThreshold, thresholdIncrease)
} deriving (Show, Eq)

class Quality a where
    getQuality :: a -> Status

normalize :: Double -> Double -> Double
normalize value weight = value / weight

instance Quality Component where
    getQuality component
        | qualityDirection component == Lower =
            if normalizedValue <= normalize (okayThreshold component) weightValue
                then Pass
                else if normalizedValue <= normalize (smallErrorThreshold component) weightValue
                    then SmallError
                    else case largeErrorThreshold component of
                        Just threshold -> if normalizedValue <= threshold
                                            then LargeError
                                            else Fail
                        Nothing -> Fail
        | qualityDirection component == Higher =
            if normalizedValue >= normalize (okayThreshold component) weightValue
                then Pass
                else if normalizedValue >= normalize (smallErrorThreshold component) weightValue
                    then SmallError
                    else case largeErrorThreshold component of
                        Just threshold -> if normalizedValue >= threshold
                                            then LargeError
                                            else Fail
                        Nothing -> Fail
        where
            weightValue = maybe 1.0 fromIntegral (weight component)
            normalizedValue = normalize (value component) weightValue


instance Quality Product where
    getQuality product
        | any isComponentFail (components product) = Fail
        | relativeThresholdMet && absoluteThresholdMet = Pass
        | otherwise = Fail
      where
        isComponentFail component = getQuality component == Fail
        totalScore = calculateTotalScore product
        errorPointsTotal = calculateErrorPointsTotal product
        relativeThresholdMet = case relativeThreshold product of
            Just (totalScoreThreshold, thresholdIncrease) ->
                totalScore >= round totalScoreThreshold + (errorPointsTotal * thresholdIncrease)
            Nothing -> True
        absoluteThresholdMet = errorPointsTotal <= absoluteThreshold product



calculateErrorPointsTotal :: Product -> Int
calculateErrorPointsTotal product =
    let componentErrors = map getComponentError (components product)
    in sum componentErrors
    where
        (smallErrorPoint, largeErrorPoint) = errorPoints product
        getComponentError :: Component -> Int
        getComponentError component
            | componentQuality component == SmallError = smallErrorPoint
            | componentQuality component == LargeError = largeErrorPoint
            | otherwise = 0
        componentQuality = getQuality

calculateTotalScore :: Product -> Int
calculateTotalScore product =
  let componentScores = map calculateComponentScore (components product)
      totalScoresAdded = sum componentScores
      productsLength = length componentScores
      totalScore = div totalScoresAdded productsLength
  in totalScore

calculateComponentScore :: Component -> Int
calculateComponentScore component =
  let normalizedValue = value component / weightValue
      weightValue = maybe 1.0 fromIntegral (weight component)
  in round (normalizedValue * 100)


tire = Component { value = 6, weight= Nothing, okayThreshold = 5, smallErrorThreshold = 10, largeErrorThreshold = Just 15, qualityDirection = Lower}
oil = Component { value = 51, weight= Nothing, okayThreshold = 50, smallErrorThreshold = 100, largeErrorThreshold = Just 150, qualityDirection = Lower}

car = Product { components = [tire, oil],errorPoints = (1, 2), absoluteThreshold = 2, relativeThreshold = Nothing}

examen1 = Component { value = 10, weight= Just 20, okayThreshold = 10, smallErrorThreshold = 8, largeErrorThreshold = Nothing, qualityDirection = Higher}
examen2 = Component { value = 8, weight= Just 20, okayThreshold = 10, smallErrorThreshold = 8, largeErrorThreshold = Nothing, qualityDirection = Higher}

semester = Product { components = [examen1, examen2], errorPoints= (2, 10), absoluteThreshold= 2, relativeThreshold = Just (50, 4)}
