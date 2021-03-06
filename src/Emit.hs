{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import "mtl" Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import JIT
import Codegen
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)

false = zero
true = one

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.UnaryDef name args body) =
  codegenTop $ S.Function ("unary" ++ name) args body

codegenTop (S.BinaryDef name args body) =
  codegenTop $ S.Function ("binary" ++ name) args body
  
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
                         -- %entry
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition
                         -- if.then
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true block
  br ifexit              -- Branch ot the merge block
  ifthen <- getBlock
                         -- if.else
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false block
  br ifexit
  ifelse <- getBlock
                         -- if.exit
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"
                                 -- %entry
  i <- alloca double
  istart <- cgen start           -- Generate Loop variable initial value
  stepval <- cgen step          -- Generate Loop variable step value
  store i istart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop block
                                 -- for.loop
  setBlock forloop
  cgen body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext
  cond <- cgen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True
  cbr test forloop forexit       -- Generate the loop condition
  setBlock forexit
  return zero

cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]

cgen (S.BinOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (S.Call ("binary" ++ op) [a,b])
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs
cgen x = error (show x)

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res of
    Right newast -> return newast
    Left err -> putStrLn err >> return oldast
  where
    modn = mapM codegenTop fns
    oldast = runLLVM mod modn

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test
