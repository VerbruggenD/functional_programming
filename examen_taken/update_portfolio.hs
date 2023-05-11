
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

stock1 = Stock "ABC" 10.0 [11.0, 12.0, 13.0] [10.0, 9.0, 8.0]
stock2 = Stock "XYZ" 100.0 [90.0, 80.0, 70.0] [100.0, 110.0, 120.0]

updatedStock1 = shiftPrice stock1
-- updatedStock1 should be:
-- Stock "ABC" 11.0 [12.0, 13.0] [11.0, 10.0, 9.0, 8.0]
updatedStock2 = shiftPrice stock2
-- updatedStock2 should be:
-- Stock "XYZ" 90.0 [80.0, 70.0] [90.0, 100.0, 110.0, 120.0, 130.0]

holding1 = Holding stock1 5 (price stock1 * 5)
holding2 = Holding stock2 10 (price stock2 * 10)


updatedHolding1 = updateHoldingValueAndPrice holding1
-- updatedHolding1 should be:
-- Holding (Stock "ABC" 11.0 [12.0,13.0] [11.0,10.0,9.0,8.0]) 5 55.0

updatedHolding2 = updateHoldingValueAndPrice holding2
-- updatedHolding2 should be:
-- Holding (Stock "XYZ" 90.0 [80.0,70.0] [90.0,100.0,110.0,120.0,130.0]) 10 900.0

initialPortfolio = Portfolio [holding1, holding2] 1000.0 (stocksValue holding1 + stocksValue holding2 + 1000.0)
-- Portfolio {{[{name = "ABC", price = 10.0, priceFuture = [11.0,12.0,13.0], priceHistory = [10.0,9.0,8.0]}, amountStocks = 5, stocksValue = 50.0},
-- {{name = "XYZ", price = 100.0, priceFuture = [90.0,80.0,70.0], priceHistory = [100.0,110.0,120.0]}, amountStocks = 10, stocksValue = 1000.0}]
-- , cashBudget = 1000.0, totalValue = 2050.0}

updatedPortfolio = updatePortfolioValueAndPrices initialPortfolio     -- totalValue of the portfolio should be 1955.0
