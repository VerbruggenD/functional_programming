import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace
import Data.Ord (comparing)
import Data.List (elemIndex, maximumBy)

{-
   ========== DEEL 1 ===========
-}

data Stock = Stock {
    name :: String,
    price :: Double,
    priceFuture :: [Double],
    priceHistory :: [Double] } deriving (Eq, Show)

data Holding = Holding {
    stock :: Stock,
    amountStocks :: Integer,
    stocksValue :: Double } deriving (Eq, Show)

data Portfolio = Portfolio {
    holdings :: [Holding],
    cashBudget :: Double,
    totalValue :: Double } deriving (Eq, Show)

data Action = Buy | Hold | Sell deriving (Eq, Show)

type Strategy = Stock -> Action

updateHolding :: Holding -> Action -> Double -> (Holding, Double)
updateHolding (Holding stock amount value) action buyBudget =
  case action of
    Buy -> (Holding stock newAmount newValue, newValue - value)
    Sell -> (Holding stock newAmount newValue, newValue - value)
    Hold -> (Holding stock amount newValueHold, newValueHold - value)
  where
    currentPrice = price stock
    maxBuyAmount = floor $ buyBudget / currentPrice
    maxSellAmount = amount
    newAmount = case action of
      Buy -> amount + maxBuyAmount
      Sell -> 0
      Hold -> amount
    newValue = fromIntegral newAmount * currentPrice
    newValueHold = value

simpleStrategy :: Strategy  -- buy when the stock is 10% lower than 3 months ago, sell when price doubled
simpleStrategy stock =
  let
    history = priceHistory stock
    currentPrice = price stock
    percentChange = if length history < 3
                    then 0
                    else (currentPrice - last (take 3 history)) / last (take 3 history)
  in
    if trace (show "price: " ++ show (price stock)) percentChange <= -0.1 then Buy
    else if percentChange >= 1 then Sell
    else Hold
    

movingAvgStrategy :: Strategy   -- buy when stock is 10% lower than moving avg of 3 months
movingAvgStrategy stock =       -- sell when price is 10% higher than this moving avg
    let
        history = take 3 (priceHistory stock)
        currentPrice = price stock
        average = sum history / fromIntegral (length history)
        percentChange = if length history < 3
                        then 0
                        else (currentPrice - average) / average
    in
        if trace (show "price: " ++ show (price stock)) percentChange <= -0.1 then Buy
        else if percentChange >= 0.1 then Sell
        else Hold

afterPeakStrategy :: Strategy   -- buy when the stock is recovering from a negative peak
afterPeakStrategy stock =       -- sell when stock is dropping from a positive peak
    let
        previousPrice = (priceHistory stock) !! 1
        secondPrevPrice = (priceHistory stock) !! 2
        currentPrice = price stock
    in
        if secondPrevPrice < previousPrice && currentPrice < previousPrice then Sell
        else if secondPrevPrice > previousPrice && currentPrice > previousPrice then Buy
        else Hold


updatePortfolioValueAndPrices :: Portfolio -> Portfolio
updatePortfolioValueAndPrices (Portfolio holdings cashBudget totalValue) =
    let updatedHoldings = map updateHoldingValueAndPrice holdings
        updatedTotalValue = sum $ map stocksValue updatedHoldings ++ [cashBudget]
    in Portfolio updatedHoldings cashBudget updatedTotalValue

updateHoldingValueAndPrice :: Holding -> Holding
updateHoldingValueAndPrice (Holding stock amountStocks stocksValue) =
    let updatedStock = shiftPrice stock
        updatedStocksValue = fromInteger amountStocks * price updatedStock
    in Holding updatedStock amountStocks updatedStocksValue

shiftPrice :: Stock -> Stock
shiftPrice (Stock name price (currentPrice:futurePrices) priceHistory) =
    Stock name currentPrice futurePrices (currentPrice:priceHistory)

updatePortfolioWithStrategy :: Portfolio -> Strategy -> Portfolio
updatePortfolioWithStrategy portfolio strategy =
    let updatedHoldingActions = map (\holding -> (holding, strategy (stock holding))) (holdings portfolio)  -- apply strategy to holdings
        buyHoldingsLength = length $ filter (\(_, action) -> action == Buy) updatedHoldingActions           -- length of the filtered buy actions for dividing budget
        buyBudgetPerStock = if buyHoldingsLength == 0 then 0    -- dividing the budget over all holdings with Buy action
                                else cashBudget portfolio / fromIntegral buyHoldingsLength

        (updatedHoldings, updatedCashBudget) = foldr (\(holding, action) (updated, budget) ->
            let (newHolding, newCost) = updateHolding holding action buyBudgetPerStock  -- applies updateHolding to every holding, which buys/sells/holds the stock
                newBudget = budget - newCost        -- update the budget with the actual spending subtracted
            in (newHolding:updated, newBudget))
            ([], cashBudget portfolio) updatedHoldingActions                -- initial situation for the foldr
        updatedTotalValue = sum $ map stocksValue updatedHoldings ++ [updatedCashBudget]

    in trace (show "cash: " ++  show updatedCashBudget) Portfolio updatedHoldings updatedCashBudget updatedTotalValue

updatePortfolioUntilNoFuture :: Portfolio -> Strategy -> Portfolio
updatePortfolioUntilNoFuture portfolio strategy =
  let updatedPortfolio = updatePortfolioWithStrategy portfolio strategy -- applying strategy on portfolio
      updatedHoldings = holdings updatedPortfolio
      futureLists = map (priceFuture . stock) updatedHoldings   -- list of the future values
      noFutureStocks = filter null futureLists  -- filter the stocks that have no future prices left
  in if length noFutureStocks == length updatedHoldings -- if no future values left
       then updatedPortfolio
       else updatePortfolioUntilNoFuture (updatePortfolioValueAndPrices updatedPortfolio) strategy  -- recursive call

-- Function to compare portfolios by totalValue
comparePortfolios :: Portfolio -> Portfolio -> Ordering
comparePortfolios p1 p2 = compare (totalValue p1) (totalValue p2)

bestPortfolioIndex :: [Portfolio] -> Maybe Int
bestPortfolioIndex [] = Nothing
bestPortfolioIndex portfolios = elemIndex best portfolios
  where
    best = maximumBy comparePortfolios portfolios

testAllStrategies :: [Strategy] -> Portfolio -> Strategy
testAllStrategies strategies portfolio =
  let
    portfolios = map (updatePortfolioUntilNoFuture portfolio) strategies
  in
    if null portfolios then error "No portfolios found"
    else strategies !! fromJust (bestPortfolioIndex portfolios)


-- TESTEN:

-- Stocks
sampleStock = Stock { name = "ABC", price = 10.0, priceHistory = [], priceFuture = [] }
sampleStock2 = Stock { name = "ABC", price = 12.5, priceHistory = [], priceFuture = [] }

-- Holdings
sampleHolding = Holding { stock = sampleStock, amountStocks = 20, stocksValue = 200.0 }
sampleHolding2 = Holding { stock = sampleStock2, amountStocks = 10, stocksValue = 125.0 }

-- Stocks
constantStock = Stock "constant" 100 [] [100, 100, 100, 100]
increasingStock = Stock "increasing" 120 [] [120, 110, 100, 90]
decreasingStock = Stock "decreasing" 90 [] [90, 100, 110, 120]
spikeStock = Stock "spike" 400 [] [400, 100, 100, 100]
dropStock = Stock "drop" 50 [] [50, 150, 150, 150]
shortHistoryStock = Stock "short history" 100 [] [100, 90]

-- result = simpleStrategy constantStock   -- should give Hold
-- result = simpleStrategy increasingStock -- should give Hold
-- result = simpleStrategy decreasingStock -- should give Buy
-- result = simpleStrategy spikeStock      -- should give Sell
-- result = simpleStrategy dropStock       -- should give Buy
-- result = simpleStrategy shortHistoryStock   -- should give Hold

testStock1 = Stock "AAPL" 120.0 [220.0, 240.0, 300.0] [120.0, 115.0, 110.0] -- spike stock (should sell)
testStock2 = Stock "GOOG" 1900.0 [2100.0, 2200.0, 2300.0] [1900.0, 1800.0, 1700.0]  -- slight increase stock (should hold)
testStock3 = Stock "MSFT" 260.0 [240.0, 230.0, 220.0] [260.0, 270.0, 280.0] -- slight decrease stock (triggers buy)
testStock4 = Stock "TSLA" 19.0 [18.0, 10.0, 9.0] [19.0, 20.0, 21.0]     -- drop stock (triggers buy)


testHolding1 = Holding testStock1 10 1250.0
testHolding2 = Holding testStock2 5 10000.0
testHolding3 = Holding testStock3 20 5000.0
testHolding4 = Holding testStock4 7 (7 * price testStock4)

testPortfolio = Portfolio [testHolding1,testHolding2, testHolding3, testHolding4] 10000.0 25000.0

updatedPortfolio = updatePortfolioUntilNoFuture testPortfolio afterPeakStrategy

strategies = [simpleStrategy, movingAvgStrategy, afterPeakStrategy] -- return van de functie ookal is er geen show gedefinieerd is voldoende

test = testAllStrategies strategies testPortfolio

{-
   ========== DEEL 2 ===========
-}

data Status = Pass | SmallError | LargeError | Fail deriving (Eq, Show)

class QualityCheck a where
  getQuality :: a -> Status

class QualityControl a where
  meetsQualityControl :: [a] -> Bool

data Exam = Exam { score :: Float, studypoints :: Int }

instance QualityCheck Exam where
  getQuality exam
    | score exam >= 10.0 = Pass
    | score exam >= 8.0 = SmallError
    | otherwise = Fail

instance QualityControl Exam where
  meetsQualityControl exams = trace (show weightedScore)
    all (\examen -> getQuality examen /= Fail) exams &&
    numSmallErrors <= 2 && numFails == 0 &&
    ((weightedScore >= 0.58 && numSmallErrors <= 2) || (weightedScore >= 0.54 && numSmallErrors <= 1))
    where
      numSmallErrors = length $ filter (\exam -> getQuality exam == SmallError) exams
      numFails = length $ filter (\exam -> getQuality exam == Fail) exams
      totalWeightedPoints = sum $ map (\exam -> score exam * fromIntegral (studypoints exam)) exams
      totalWeightedCredits = sum $ map (fromIntegral . studypoints) exams
      weightedScore = totalWeightedPoints / (20 * totalWeightedCredits)

examen1 = Exam { score = 15.5, studypoints = 2 }
examen2 = Exam { score = 10.2, studypoints = 1 }
examen3 = Exam { score = 9.5, studypoints = 3 } -- kleine afwijking
examen4 = Exam { score = 12.0, studypoints = 5 }
examen5 = Exam { score = 6.7, studypoints = 2 } -- fail

examens1 = [examen1, examen2, examen5] -- 1 fail
examens2 = [examen1, examen3, examen4] -- kleine afwijking percentage goed
examens3 = [examen2, examen3] -- kleine afwijking percentage slecht
examens4 = [examen1, examen2, examen4] -- alles goed

data ExamQuestion = ExamQuestion { question :: String, answer :: String, isSeriousMistake :: Bool } deriving (Eq, Show)

data ExamAnswer = ExamAnswer { givenAnswer :: String, examQuestion :: ExamQuestion } deriving (Eq, Show)

instance QualityCheck ExamAnswer where
  getQuality examAnswer
    | givenAnswer examAnswer == answer (examQuestion examAnswer) = Pass
    | isSeriousMistake (examQuestion examAnswer) = LargeError
    | otherwise = SmallError

instance QualityControl ExamAnswer where
  meetsQualityControl answers = trace (show totalPoints)
    totalPoints <= 9
    where
      totalPoints = sum $ map calculatePoints answers
      calculatePoints answer
        | getQuality answer == LargeError = 5
        | getQuality answer == SmallError = 1
        | otherwise = 0

-- heavy mistakes
question1 = ExamQuestion { question = "Question 1", answer = "A", isSeriousMistake = True }
answer1 = ExamAnswer { givenAnswer = "C", examQuestion = question1 }
question2 = ExamQuestion { question = "Question 5", answer = "C", isSeriousMistake = True }
answer2 = ExamAnswer { givenAnswer = "B", examQuestion = question2 }

-- small mistake
question3 = ExamQuestion { question = "Question 2", answer = "C", isSeriousMistake = False }
answer3 = ExamAnswer { givenAnswer = "B", examQuestion = question3 }
question4 = ExamQuestion { question = "Question 3", answer = "B", isSeriousMistake = False }
answer4 = ExamAnswer { givenAnswer = "C", examQuestion = question4 }

-- right answers
question5 = ExamQuestion { question = "Question 1", answer = "A", isSeriousMistake = True }
answer5 = ExamAnswer { givenAnswer = "A", examQuestion = question5 }
question6 = ExamQuestion { question = "Question 2", answer = "C", isSeriousMistake = False }
answer6 = ExamAnswer { givenAnswer = "C", examQuestion = question6 }
question7 = ExamQuestion { question = "Question 4", answer = "A", isSeriousMistake = False }
answer7 = ExamAnswer { givenAnswer = "A", examQuestion = question7 }

drivingExam1 = [answer1, answer2, answer5, answer6, answer7] -- two heavy mistakes
drivingExam2 = [answer1, answer5, answer6, answer7] -- one heavy mistakes
drivingExam3 = [answer3, answer4, answer3, answer4, answer3, answer4, answer3, answer4] -- 8 small mistakes
drivingExam4 = [answer3, answer4, answer3, answer4, answer3, answer4, answer3, answer4, answer3, answer4] -- 10 small mistakes
drivingExam5 = [answer5, answer6, answer7, answer6, answer5, answer6, answer7, answer5, answer6, answer5, answer7, answer5, answer6] -- 13 right answers

data ServiceInterval = ServiceInterval { smallError :: Int, largeError :: Int, failure :: Int }

data MachineComponent = MachineComponent { interval :: ServiceInterval, hoursInUse :: Int, lastMaintenance :: Int }

instance QualityCheck MachineComponent where
  getQuality component
    | hours <= smallError (interval component) = Pass
    | hours <= largeError (interval component) = SmallError
    | hours <= failure (interval component) = LargeError
    | otherwise = Fail
    where
      hours = hoursInUse component - lastMaintenance component

instance QualityControl MachineComponent where
  meetsQualityControl components =
    totalErrorPoints <= 2 && all (\component -> getQuality component /= Fail) components
    where
      totalErrorPoints = sum $ map calculateErrorPoints components
      calculateErrorPoints component
        | getQuality component == LargeError = 2
        | getQuality component == SmallError = 1
        | otherwise = 0

failComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 9, lastMaintenance = 2}
smallErrorComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 5, lastMaintenance = 2}
largeErrorComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 7, lastMaintenance = 2}
goodComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 3, lastMaintenance = 2 }

machine1 = [failComp, goodComp] -- should fail
machine2 = [smallErrorComp, goodComp] -- should pass
machine3 = [largeErrorComp, goodComp] -- should pass
machine4 = [smallErrorComp, goodComp, smallErrorComp] -- should pass
machine5 = [largeErrorComp, goodComp, smallErrorComp] -- should fail
