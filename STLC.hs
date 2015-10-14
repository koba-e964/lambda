{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module STLC where

import qualified Data.Map as Map
import Control.Monad.State

type Name = String

data LambdaExprInfo a
  = LEApp (LambdaExprInfo a) (LambdaExprInfo a) a
  | LEAbst Name (LambdaExprInfo a) a
  | LEVar Name a
  deriving (Eq)
getInfo :: LambdaExprInfo a -> a
getInfo (LEApp _ _ a) = a
getInfo (LEAbst _ _ a) = a
getInfo (LEVar _ a) = a

type LambdaExpr = LambdaExprInfo ()
type TypedLambdaExpr = LambdaExprInfo LambdaType

instance Show LambdaExpr where
  show x = case x of
    LEApp x y _ -> showParen x ++ " " ++ showParen y
    LEAbst x e _ -> "\\" ++ x ++ ". " ++ show e
    LEVar x _ -> x
    where
    showParen s@(LEVar {}) = show s
    showParen x =  "(" ++ show x ++ ")"
instance Show TypedLambdaExpr where
  show x = case x of
    LEApp x y a -> showParen x ++ " " ++ showParen y ++ " : " ++ show a
    LEAbst x e (LTArrow a b) -> "\\" ++ x ++ ":" ++ show a ++ ". " ++ showParen e ++ " : " ++ show b
    LEAbst _ _ _ -> error "non-arrow type in lambda abstraction"
    LEVar x a -> x
    where
    showParen s@(LEVar {}) = show s
    showParen x =  "(" ++ show x ++ ")"




-- | Lambda expression represented with de Bruijn index term
data DeBruijn
  = DBApp DeBruijn DeBruijn
  | DBAbst DeBruijn
  | DBInt Int
  deriving (Eq, Show)

data LambdaType = 
  LTBase | LTArrow LambdaType LambdaType | LTVar Int
  deriving (Eq)

instance Show LambdaType where
  show x = case x of
    LTBase -> "o"
    LTArrow x y -> showParen x ++ " -> " ++ show y
    LTVar i -> "t" ++ show i
    where
     showParen a@(LTArrow x y) = "(" ++ show a ++ ")"
     showParen x = show x
     


type TypeSubst = Map.Map Int LambdaType
type Constraint = (LambdaType, LambdaType)

typeTerm :: LambdaExpr -> Maybe TypedLambdaExpr
typeTerm expr = do
  let (tle, cons) = assignTypeVars expr
  te <- unifyTypes cons
  return $ replaceTypes te tle


assignTypeVars :: LambdaExpr -> (TypedLambdaExpr, [Constraint])
unifyTypes :: [Constraint] -> Maybe TypeSubst
replaceTypes :: TypeSubst -> TypedLambdaExpr -> TypedLambdaExpr


assignTypeVars expr = flip evalState 0 $ go expr Map.empty
  where
    go (LEApp m n _) env = do
      x <- get
      let y = x + 1
      modify (+ 2)
      (mte, mcons) <- go m env
      (nte, ncons) <- go n env
      let mty = getInfo mte
      let nty = getInfo nte
      return ((LEApp mte nte (LTVar y)), [(mty, LTArrow (LTVar x) (LTVar y)), (nty, LTVar x)] ++ mcons ++ ncons)
    go (LEAbst xvar m _) env = do
      x <- get
      modify (+ 1)
      let newenv = Map.insert xvar (LTVar x) env
      (mte, mcons) <- go m newenv
      let mty = getInfo mte
      return ((LEAbst xvar mte (LTArrow (LTVar x) mty)), mcons)
    go (LEVar var _) env = do
      let ty = env Map.! var
      return (LEVar var ty, [])

unifyTypes ls = go Map.empty ls
  where
    go env [] = return env
    go env ((x, y):rest) | x == y = go env rest
    go env ((LTVar i1, x):rest)
      | Map.member i1 env = go env ((env Map.! i1, x):rest)
      | otherwise = let v = substType env x in
          go (Map.insert i1 v (Map.map (substituteOne i1 v) env)) rest
    go env ((x, LTVar i2):rest) = go env ((LTVar i2, x):rest)
    go env ((LTArrow t1 t2, LTArrow t3 t4):rest) = go env ((t1, t3):(t2, t4):rest)
    go env ((LTBase, LTBase):rest) = go env rest
    go env ((_, _):rest) = Nothing

substituteOne :: Int -> LambdaType -> LambdaType -> LambdaType
substituteOne tvar trep ty = go ty
  where
    go (LTArrow a b) = LTArrow (go a) (go b)
    go LTBase = LTBase
    go (LTVar x)
      | x == tvar = trep
      | otherwise = LTVar x


replaceTypes subst expr = go expr
  where
    go (LEApp a b ty) = LEApp (go a) (go b) (substType subst ty)
    go (LEAbst x e ty) = LEAbst x (go e) (substType subst ty)
    go (LEVar x ty) = LEVar x (substType subst ty)

substType :: TypeSubst -> LambdaType -> LambdaType
substType subst = rep
  where
    rep LTBase = LTBase
    rep (LTArrow x y) = LTArrow (rep x) (rep y)
    rep (LTVar i)
      | Map.member i subst = subst Map.! i
      | otherwise = LTVar i


leToDb :: LambdaExpr -> DeBruijn
leToDb _ = undefined
  
