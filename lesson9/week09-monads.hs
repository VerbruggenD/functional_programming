import Data.Maybe

data Expr = Cte Float 
          | Deling Expr Expr deriving (Eq, Show)
          
eval :: Expr -> Float
eval (Cte n) = n
eval (Deling e1 e2) = (eval e1) / (eval e2)

data Exception = Exception {message :: String,
                            expression :: String} 
                deriving (Eq, Show) 

eval2 :: Expr -> (Maybe Exception, Float)
eval2 (Cte n) = (Nothing, n)
eval2 (Deling e1 e2) = 
   let (exc1, value1) = eval2 e1
       ex1 = fromJust exc1
       (exc2, value2) = eval2 e2
       ex2 = fromJust exc2
       result = value1 / value2
       divisionByZero = Exception "division by zero" (show (Deling e1 e2))
       completeMessage = message ex1 ++ message ex2
       completeExpr = expression ex1 ++ expression ex2
       completeException = Exception completeMessage completeExpr
   in if (isNothing exc1) && (isNothing exc2) 
            then 
                if value2 == 0 then (Just divisionByZero, result)
                              else (Nothing, result)
            else if (isNothing exc1) 
                       then (exc2, result)                         
                       else if (isNothing exc2) then (exc1, result)   
                                                else (Just completeException, result)
                                                
                                                
eval3 :: Expr -> (Float, String)
eval3 (Cte n) = (n, "[" ++ (show n) ++ " => " ++ (show n) ++ "]")
eval3 (Deling e1 e2) = 
   let (value1, state1) = eval3 e1
       (value2, state2) = eval3 e2
       result = value1 / value2
       newState ="["++state1++"/"++state2++ " => " ++ (show result) ++ "]"
   in (result, newState)

eval4 :: Expr -> Maybe Float
eval4 (Cte n) = Just n
eval4 (Deling e1 e2) = 
    if (eval4 e2) == Just 0 
        then Nothing
        else Just (fromJust (eval4 e1) / (fromJust (eval4 e2)))

-- eval4 werkt voor Deling 3 0, maar niet voor 
--           Deling (Deling 3 0) 4
--           Deling 4 (Deling 3 0)



veiligeDeling teller 0 = Nothing
veiligeDeling teller noemer = Just (teller / noemer)

eval5 :: Expr -> Maybe Float
eval5 (Cte n) = Just n
eval5 (Deling e1 e2) = 
    if (eval5 e2) == Nothing 
        then Nothing
        else veiligeDeling (fromJust (eval5 e1)) (fromJust (eval5 e2))

-- eval5 werkt even goed als eval4, maar met functie die ‘lift’ 
--           is gelijk aan klaarstomen voor >>=

eval6 :: Expr -> Maybe Float
eval6 (Cte n) = Just n
eval6 (Deling e1 e2) = 
    case (eval6 e2) of
        Nothing -> Nothing
        Just m  -> veiligeDeling (fromJust (eval6 e1)) m

-- eval6 werkt even goed als eval5, 
--           maar Nothing/Just explicieter zichtbaar
-- we moeten nog matchen met de eerste parameter


eval7 :: Expr -> Maybe Float
eval7 (Cte n) = Just n
eval7 (Deling e1 e2) = 
    case (eval7 e1) of
        Nothing -> Nothing
        Just n  -> case (eval7 e2) of
                      Nothing -> Nothing
                      Just m  -> veiligeDeling n m
              
eval8 :: Expr -> Maybe Float
eval8 (Cte n) = Just n
eval8 (Deling e1 e2) = 
    eval8 e1 >>= \n ->
    eval8 e2 >>= \m -> 
    veiligeDeling n m 


----veiligeDeling' teller 0 = Nothing
--veiligeDeling' teller noemer = return (teller / noemer)

eval9 (Cte n) = return n
eval9 (Deling e1 e2) = 
    eval9 e1 >>= \n ->
    eval9 e2 >>= \m -> 
    veiligeDeling n m 
              
                      
