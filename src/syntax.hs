module Syntax where

type Name = String

data Expr = Float Double
          | BinOp Name Expr Expr
          | UnaryOp Name Expr
          | BinaryDef Name [Name] Expr
          | UnaryDef Name [Name] Expr
          | Var String
          | Call Name [Expr]
          | Function Name [Name] Expr
          | Extern Name [Name]
          | If Expr Expr Expr
          | For Name Expr Expr Expr Expr
          deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)

