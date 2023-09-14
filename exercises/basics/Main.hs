module Main where
import Text.Read.Lex (Number)

numOfSol :: Float -> Float -> Float -> Float
calcQuadraticPos :: Float -> Float -> Float -> Float
calcQuadraticNeg :: Float -> Float -> Float -> Float

numOfSol a b c = do
  let pos = calcQuadraticPos a b c;
  let neg = calcQuadraticNeg a b c;
  if isNaN pos && isNaN neg
    then 0
  else if isNaN pos || isNaN neg
    then 1
  else 2

calcQuadraticPos a b c = (-b + sqrt ((b^2) - 4 * a * c)) / (2 * a)
calcQuadraticNeg a b c = (-b - sqrt ((b^2) - 4 * a * c)) / (2 * a)


calcPow :: Int -> Int -> Int

calcPow x 0 = 1
calcPow x n 
  | even n = (x^(div n 2))^2
  | otherwise = x * calcPow x (n-1)

main = print "Hello World"



