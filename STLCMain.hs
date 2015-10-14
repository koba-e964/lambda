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

main :: IO ()
main = do
  let lid = LEAbst "x" (LEVar "x" ()) ()
  print $ fromJust $ typeTerm (LEApp lid lid ())
  let t3 = LEApp (LEApp lid lid ()) lid ()
  print $ fromJust $ typeTerm t3
  let tt = LEAbst "x" (LEAbst "y" (LEVar "x" ()) ()) ()
  let ff = LEAbst "x" lid ()
  putStrLn $ "tt:" ++ show (typeTerm tt)
  putStrLn $ "ff:" ++ show (typeTerm ff)
  putStrLn $ "1:" ++ show (typeTerm $ natToLT 1)
  putStrLn $ "2:" ++ show (typeTerm $ natToLT 2)
  putStrLn $ "2 2:" ++ show (typeTerm $ LEApp (natToLT 2) (natToLT 2) ())
  let not = labst "b" (labst "x" (labst "y" (lapp (lapp (lvar "b") (lvar "y")) (lvar "x"))))
  putStrLn $ "not:" ++ show (typeTerm not)
