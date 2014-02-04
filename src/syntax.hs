module Syntax where

type Name = String

data Expr = Float Double
          | BinOp Name Expr Expr
          | Var String
          | Call Name [Expr]
          | Function Name [Name] Expr
          | Extern Name [Name]
          | UnaryOp Name Expr
          deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)
