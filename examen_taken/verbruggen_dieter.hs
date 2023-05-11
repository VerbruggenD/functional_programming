import Data.Maybe (fromMaybe)
import Distribution.PackageDescription (BuildType(Simple))

{-
    taak 1: hogere orde
    - simuleren van beleggen
    - men krijgt:
        - verzameling van aandeel koersen
        - budget
    - types:
        - aandeel: naam, prijs, hoeveelheid
    - functie:
        - op basis van lijst met koersen zeggen kopen/houden/verkopen
            - dit is de strategie, meegeven wanneer verkopen en wanneer aankopen
            - strategie voor zowel als men het aandeel al heeft of als men het wilt kopen
        - kopen/verkopen
            - bij kopen budget volledig benutten
            - budget verandert mee met kopen/verkopen
-}

-- stock types

data Stock = Stock {
    name :: String,
    price :: Double,
    priceHistory :: [Double] } deriving Show

-- portfolio type

data Portfolio = Portfolio { stocks :: [(Stock, Double)], cash :: Double } deriving Show

        -- examples

appleStock = Stock { name = "AAPL", price = 133.50, priceHistory = [100.0, 120.0] }
microsoftStock = Stock { name = "MSFT", price = 235.75, priceHistory = [] }

voorbeeldPortfolio = Portfolio { stocks = [(appleStock, 3), (microsoftStock, 2)], cash = 1000.0 }

data Action = Buy | Hold | Sell deriving (Eq, Show)

type Strategy = Stock -> Action

evolutionStrategy :: Int -> Stock -> Action
evolutionStrategy n stock =
    let
        priceHistory' = take n (priceHistory stock)
        [price1, price2] = take 2 priceHistory'
        pctChange = (price2 - price1) / price1
    in
        if pctChange >= 0.1 then Buy
        else if pctChange >= 0.0 then Hold
        else if pctChange >= -0.5 then Sell
        else Hold

updatePortfolio :: Portfolio -> Strategy -> Portfolio
updatePortfolio (Portfolio stocks cash) strategy =
    let
        (updatedStocks, updatedCash) = foldr applyStrategy ([], cash) stocks
    in Portfolio updatedStocks updatedCash
    where
        applyStrategy :: (Stock, Double) -> ([(Stock, Double)], Double) -> ([(Stock, Double)], Double)
        applyStrategy (stock@Stock { price = currentPrice }, qty) (accStocks, accCash) =
            case strategy stock of
                Buy ->
                    let
                        cost = qty * currentPrice
                    in ((stock, qty) : accStocks, accCash - cost)
                Sell ->
                    let
                        value = qty * currentPrice
                    in ((stock, 0) : accStocks, accCash + value)
                Hold ->
                    ((stock, qty) : accStocks, accCash)


-- function for updating price history

updatePrice :: Double -> Stock -> Stock
updatePrice newPrice stock = 
  let updatedHistory = priceHistory stock ++ [newPrice]
  in stock { price = newPrice, priceHistory = updatedHistory }

main :: IO ()
main = do
  let stock1 = Stock { name = "AAPL", price = 100.0, priceHistory = [100.0, 110.0, 120.0, 130.0, 140.0, 150.0] }
      stock2 = Stock { name = "GOOG", price = 200.0, priceHistory = [200.0, 190.0, 180.0, 190.0, 200.0, 210.0] }
      portfolio = Portfolio { stocks = [(stock1, 10.0), (stock2, 20.0)], cash = 100.0 }
  putStrLn $ "Initial portfolio: " ++ show portfolio
  let updatedPortfolio = updatePortfolio portfolio (evolutionStrategy 2)
  putStrLn $ "Updated portfolio: " ++ show updatedPortfolio