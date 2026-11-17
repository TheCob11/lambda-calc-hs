module Untyped where
import Data.Function (on)

type Name = String

data Expr =
    Var Name
  | Lam Name Expr
  | App Expr Expr
    deriving (Show)

pretty :: Expr -> String
pretty (Var s)    = s
pretty (Lam s e1) = "\\" ++ s ++ ". " ++ pretty e1
pretty (App (Var s1) (Var s2)) = s1 ++ " " ++ s2
pretty (App (Var s1) e2) = s1 ++ " (" ++ pretty e2 ++ ")"
pretty (App e1 (Var s2)) = "(" ++ pretty e1 ++ ")" ++ " " ++ s2
pretty (App e1 e2) = "(" ++ pretty e1 ++ ") (" ++ pretty e2 ++ ")"

subst :: Name -> Expr -> Expr -> Expr
subst name val (Var other)  = if other == name then val else Var other
subst name val (Lam x body) = Lam x (subst name val body)
subst name val (App e1 e2)  = (App `on` subst name val) e1 e2
-- subst name val (App e1 e2)  = App (subst name val e1) (subst name val e2)

reduce :: Expr -> Expr
reduce (App (Lam x body) e2) = subst x (reduce e2) (reduce body)
reduce x = x
-- reduce (Var x) = Var x
-- reduce (Lam name e) = Lam name e
-- reduce (App e1 e2) = App e1 e2

exY :: Expr
exY = Lam "f" $ App (Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))) (Lam "x" $ App (Var "f") (App (Var "x") (Var "x")))