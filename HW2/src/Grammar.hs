module Grammar where

import Data.List

data Expr = Abstraction String Expr
          | Application Expr Expr
          | Var String deriving (Ord, Eq)

instance Show Expr where
    show (Abstraction a b) = "(\\" ++ a ++ "." ++ show b ++ ")"
    show (Application a b) = "(" ++ show a ++  " " ++ show b ++ ")"
    show (Var variable)    = variable