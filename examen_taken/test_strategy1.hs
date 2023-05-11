import Test.Hspec
import Control.Monad (forM_)
import Debug.Trace

data Stock = Stock {
    name :: String,
    price :: Double,
    priceFuture :: [Double],
    priceHistory :: [Double] } deriving Show

data Holding = Holding {
    stock :: Stock,
    amountStocks :: Integer,
    stocksValue :: Double } deriving Show

data Portfolio = Portfolio {
    holdings :: [Holding],
    cashBudget :: Double,
    totalValue :: Double } deriving Show

data Action = Buy | Hold | Sell deriving (Eq, Show)

type Strategy = Stock -> Action

-- define the test data for the different edge cases
constantStock = Stock "constant" 100 [] [100, 100, 100, 100]
increasingStock = Stock "increasing" 120 [] [120, 110, 100, 90]
decreasingStock = Stock "decreasing" 90 [] [90, 100, 110, 120]
spikeStock = Stock "spike" 400 [] [400, 100, 100, 100]
dropStock = Stock "drop" 50 [] [50, 150, 150, 150]
shortHistoryStock = Stock "short history" 100 [] [100, 90]

-- define the investment strategy for the tests
testStrategy :: Strategy
testStrategy stock =
  let
    history = priceHistory stock
    currentPrice = price stock
    percentChange = if length history < 3
                    then 0
                    else (currentPrice - last (take 3 history)) / last (take 3 history)
  in
    if percentChange <= -0.1 then Buy
    else if percentChange >= 1 then Sell
    else Hold

-- define the tests for each edge case
tests :: [SpecWith ()]
tests =
  [ describe "Investment strategy" $ do
      context "when price is constant" $ do
        it "should hold" $ do
          let action = testStrategy constantStock
          action `shouldBe` Hold

      context "when price is decreasing" $ do
        it "should buy" $ do
          let action = testStrategy decreasingStock
          action `shouldBe` Buy

      context "when price is increasing, but not enough" $ do
        it "should hold" $ do
          let action = testStrategy increasingStock
          action `shouldBe` Hold

      context "when price spikes" $ do
        it "should sell" $ do
          let action = testStrategy spikeStock
          action `shouldBe` Sell

      context "when price drops suddenly" $ do
        it "should buy" $ do
          let action = testStrategy dropStock
          action `shouldBe` Buy

      context "when price history is short" $ do
        it "should hold" $ do
          let action = testStrategy shortHistoryStock
          action `shouldBe` Hold
  ]



-- run the tests
main :: IO ()
main = hspec $ sequence_ tests