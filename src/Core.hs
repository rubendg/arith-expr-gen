{-# LANGUAGE FlexibleContexts, DeriveFunctor, UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}
module Core where

import Common

type Algebra f a = f a -> a

newtype Fix f = Fx (f (Fix f))

instance (Show (f (Fix f))) => Show (Fix f) where
  show (Fx f) = show f

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

data ExprF r = 
    Add r r
  | Sub r r
  | Mul r r
  | Div r r
  | IntV Int
  | DoubleV Double
  deriving (Functor)

type Expr = Fix ExprF

evalAlg (a `Add` b) = a + b
evalAlg (a `Sub` b) = a - b
evalAlg (a `Mul` b) = a * b
evalAlg (a `Div` b) = a / b
evalAlg (IntV a) = fromIntegral a
evalAlg (DoubleV a) = a

roundDoubles :: Int -> Expr -> Expr
roundDoubles n = cata roundAlg
  where
  roundAlg (DoubleV a) = Fx $ DoubleV (roundToDec n a)
  roundAlg x = Fx x

{-

Suppose:

4 + 5 * 6 / 5

The expression generator does not take into account operator precedence.

The AST generated for the above expression is:

             (/)
             / \
            *   5
           / \
          +   6
         / \
        4   5

Evaluating this AST will result in the wrong answer:

((4 + 5) * 6) / 5 = 10.8

While the correct answer should be:

4 + (5 * 6) / 5 = 10.0

Hence instead complicating the tree generation we can write an
AST transformation that will fix the operator precedences.

Note: I've not yet figured out how to make this operation more
efficient. Currently, each rewrite operation requires in the
worst case an entire AST traversal. I'm also not entirely sure
about the convergence of the rewrite operations.
-}
fixOpPrecedence :: Expr -> Expr
fixOpPrecedence expr = 
  let (next,isNew) = rewrite (unFix expr)
  in 
    if isNew
      then fixOpPrecedence (Fx next)
      else Fx next

  where 

  rewrite ((Fx (a `Add` b)) `Mul` c) = (a `Add` (Fx (b `Mul` c)), True)
  rewrite ((Fx (a `Add` b)) `Div` c) = (a `Add` (Fx (b `Div` c)), True)
  rewrite ((Fx (a `Sub` b)) `Mul` c) = (a `Sub` (Fx (b `Mul` c)), True)
  rewrite ((Fx (a `Sub` b)) `Div` c) = (a `Sub` (Fx (b `Div` c)), True)
  
  rewrite (a `Mul` (Fx (b `Add` c))) = ((Fx (a `Mul` b)) `Add` c, True)
  rewrite (a `Div` (Fx (b `Add` c))) = ((Fx (a `Div` b)) `Add` c, True)
  rewrite (a `Mul` (Fx (b `Sub` c))) = ((Fx (a `Mul` b)) `Sub` c, True)
  rewrite (a `Div` (Fx (b `Sub` c))) = ((Fx (a `Div` b)) `Sub` c, True)

  rewrite (a `Add` b) = 
    let (a', isNewA) = rec a
        (b', isNewB) = rec b
    in (a' `Add` b', isNewA || isNewB)

  rewrite (a `Sub` b) = 
    let (a', isNewA) = rec a
        (b', isNewB) = rec b
    in (a' `Sub` b', isNewA || isNewB)

  rewrite (a `Mul` b) = 
    let (a', isNewA) = rec a
        (b', isNewB) = rec b
    in (a' `Mul` b', False || isNewA || isNewB)

  rewrite (a `Div` b) = 
    let (a', isNewA) = rec a
        (b', isNewB) = rec b
    in (a' `Div` b', False || isNewA || isNewB)

  rewrite x@(IntV _) = (x, False)
  rewrite x@(DoubleV _) = (x, False)

  rec = (\(a,b) -> (Fx a, b)) . rewrite . unFix

eval :: Expr -> Double
eval = cata evalAlg . fixOpPrecedence

instance (Show r) => Show (ExprF r) where
  show (Add a b) = show a ++ " + " ++ show b
  show (Mul a b) = show a ++ " * " ++ show b
  show (Div a b) = show a ++ " / " ++ show b
  show (Sub a b) = show a ++ " - " ++ show b
  show (IntV a)   = show a
  show (DoubleV a)   = show a
