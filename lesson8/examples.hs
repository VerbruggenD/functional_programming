import Distribution.TestSuite (TestInstance(name))
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

nietNodig = fib 170

welNodig = 3 + 4

f a b = b

oneindigeLijst n = n:oneindigeLijst (n+1)

somEersteDrie n = a+b+c
    where (a:b:c:xs) = oneindigeLijst n

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

