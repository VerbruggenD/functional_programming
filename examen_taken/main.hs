data Stock = Stock {
    name :: String,
    price :: Double,
    priceHistory :: [Double] } deriving Show

data Portfolio = Portfolio { stocks :: [(Stock, Integer)], cash :: Double } deriving Show

data Action = Buy | Hold | Sell deriving (Eq, Show)

type Strategy = Stock -> Action

updateStock :: (Stock, Integer) -> Double -> Strategy -> (Stock, Integer, Double)
updateStock (stock, quantity) cash strategy =
    case strategy stock of
        Buy -> if (cash - price stock) >= 0 then let newQuantity = round (cash / price stock) :: Integer
                                                     updatedCash = cash - (fromInteger newQuantity * price stock)
                                                 in (stock, quantity + newQuantity, updatedCash)
                                            else (stock, quantity, cash)
        Hold -> (stock, quantity, cash)
        Sell -> let soldQuantity = quantity
                    updatedCash = cash + (fromInteger soldQuantity * price stock)
               in (stock, 0, updatedCash)

updatePortfolio :: Portfolio -> Strategy -> Portfolio
updatePortfolio (Portfolio stocks cash) strategy =
    let
        (updatedStocks, updatedCash) = foldl (\(newStocks, updatedCash) stock -> let (updatedStock, updatedQuantity, updatedCash') = updateStock stock updatedCash strategy
                                                                                     in (newStocks ++ [ (updatedStock, updatedQuantity) ], updatedCash'))
                                              ([], cash) 
                                              stocks
    in
        Portfolio updatedStocks updatedCash

-- updatePortfolio :: Portfolio -> Strategy -> Portfolio
-- updatePortfolio (Portfolio stocks cash) strategy =
--     let
--         updateStock (stock, quantity) =
--             case strategy stock of
--                 Buy -> let newQuantity = quantity + (cash / price stock)
--                            cash = cash - (quantity * price stock)
--                        in (stock, newQuantity)
--                 Hold -> (stock, quantity)
--                 Sell -> let newQuantity = 0
--                             cash = cash + (quantity * price stock) 
--                         in (stock, newQuantity)
--         updatedStocks = map updateStock stocks
--     in
--         Portfolio updatedStocks cash


evolutionStrategy :: Int -> Stock -> Action
evolutionStrategy n stock =
    let
        priceHistory' = take n (priceHistory stock)    -- reduce history to the size n
        price1 = last priceHistory'                             -- last element of list is now the earliest price
        price2 = head priceHistory'                             -- first element of list is now the latest price
        priceChange = (price2 - price1) / price1
    in
        if priceChange <= -0.1 then Buy
        else if priceChange >= 1 then Sell
        else Hold

main :: IO ()
main = do
  let stock1 = Stock { name = "AAPL", price = 80.0, priceHistory = [80.0, 110.0, 120.0, 130.0, 140.0, 150.0] }
      stock2 = Stock { name = "GOOG", price = 200.0, priceHistory = [210.0, 100.0, 180.0, 190.0, 200.0, 250.0] }
      portfolio = Portfolio { stocks = [(stock1, 10), (stock2, 20)], cash = 100.0 }
  putStrLn $ "Initial portfolio: " ++ show portfolio
  let updatedPortfolio = updatePortfolio portfolio (evolutionStrategy 2)
  putStrLn $ "Updated portfolio: " ++ show updatedPortfolio


