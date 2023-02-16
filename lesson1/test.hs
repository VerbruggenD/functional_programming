import Debug.Trace

isPriemTot10 2 = True
isPriemTot10 3 = True
isPriemTot10 5 = True
isPriemTot10 7 = True
isPriemTot10 _ = False

isLeeg [] = True
isLeeg _ = False

hoofd (x:xs) = x
staart (x:xs) = xs

begintMet x [] = False
begintMet x (y:ys) =
    if x == y then True
                else False

eindigtMet x [] = False
eindigtMet x [y] = x == y
eindigtMet x (y:ys) = eindigtMet x ys

aantalKeer x [] = 0
aantalKeer x (y:ys) =
    if x == y then 1 + aantalKeer x ys
                else aantalKeer x ys

{-
    in java: 
public int aantalKeer(x, xs) {
    int teller = 0;
    for (int i = 0; i < xs.length; i++) {
        if (x == xs[i]) {
            teller ++;
        }
    }
    return teller;
}
-}

{- 
debuggen met trace
-}

som [] = 0
som (x:xs) = trace ((show x) ++ "-" 
                    ++ (show xs) ) (x + som xs)

fac 1 = 1
fac n = n * fac(n-1)