module Main where

import STLC
import Data.Maybe (fromJust)

natToLT :: Int -> LambdaExpr
natToLT n = LEAbst "f" (LEAbst "x" (go n) ()) ()
  where
    go 0 = LEVar "x" ()
    go n = LEApp (LEVar "f" ()) (go (n - 1)) ()

lapp x y = LEApp x y ()
labst x y = LEAbst x y ()
lvar x = LEVar x ()
lid = labst "x" (lvar "x")

varb = lvar "b"
varx = lvar "x"
vary = lvar "y"

main :: IO ()
main = do
  print $ fromJust $ typeTerm (LEApp lid lid ())
  let t3 = lapp (lapp lid lid) lid
  print $ fromJust $ typeTerm t3
  let tt = labst "x" (labst "y" varx)
  let ff = labst "x" lid
  putStrLn $ "tt:" ++ show (typeTerm tt)
  putStrLn $ "ff:" ++ show (typeTerm ff)
  putStrLn $ "1:" ++ show (typeTerm $ natToLT 1)
  putStrLn $ "2:" ++ show (typeTerm $ natToLT 2)
  putStrLn $ "2 2:" ++ show (typeTerm $ lapp (natToLT 2) (natToLT 2))
  let not = labst "b" (labst "x" (labst "y" (lapp (lapp varb vary) varx)))
  putStrLn $ "not:" ++ show (typeTerm not)
  print $ cbvReduce (lapp lid lid)
  print $ cbvReduce $ (lapp (lapp ((natToLT 2)) not) tt)
  print $ cbvReduce $ lapp (labst "x" (labst "y" varx)) vary
  print $ cbnReduce (lapp lid lid)
  print $ cbnReduce $ (lapp (lapp ((natToLT 2)) not) tt)
  print $ cbnReduce $ lapp (labst "x" (labst "y" varx)) vary
  let omega = labst "x" (lapp (lvar "x") (lvar "x"))
  print $ cbnReduce (lapp (labst "x" lid) (lapp omega omega))
  putStrLn $ "omega:" ++ show (typeTerm omega)

