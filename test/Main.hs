{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import AST
import TypeChecker
import Data.Text (Text)
import qualified Data.Text as T

-- | Helper function to create a location
loc :: Int -> Int -> Location
loc line col = Location line col

-- | Test cases for literal expressions
testLiterals :: TestTree
testLiterals = testGroup "Literal Expressions"
  [ testCase "Integer literal" $ do
      let expr = mkIntLit 42 (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TInt

  , testCase "String literal" $ do
      let expr = mkStrLit "hello" (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TString

  , testCase "Boolean literal" $ do
      let expr = mkBoolLit True (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TBool

  , testCase "Null literal" $ do
      let expr = mkNullLit (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TNull
  ]

-- | Test cases for binary operations
testBinaryOps :: TestTree
testBinaryOps = testGroup "Binary Operations"
  [ testCase "Integer addition" $ do
      let expr = mkBinOp "+" (mkIntLit 1 (loc 1 1)) (mkIntLit 2 (loc 1 3)) (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TInt

  , testCase "String concatenation" $ do
      let expr = mkBinOp "+" (mkStrLit "hello" (loc 1 1)) (mkStrLit "world" (loc 1 8)) (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TString

  , testCase "Boolean comparison" $ do
      let expr = mkBinOp "==" (mkBoolLit True (loc 1 1)) (mkBoolLit False (loc 1 6)) (loc 1 1)
      result <- typeCheckExpr expr
      result @?= TBool

  , testCase "Invalid operation" $ do
      let expr = mkBinOp "+" (mkIntLit 1 (loc 1 1)) (mkStrLit "hello" (loc 1 3)) (loc 1 1)
      result <- typeCheckExpr expr
      assertBool "Should fail" $ case result of
        Left _ -> True
        Right _ -> False
  ]

-- | Test cases for variable declarations
testVarDecls :: TestTree
testVarDecls = testGroup "Variable Declarations"
  [ testCase "Valid integer declaration" $ do
      let stmt = mkVarDecl "x" TInt (mkIntLit 42 (loc 1 1)) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False

  , testCase "Type mismatch in declaration" $ do
      let stmt = mkVarDecl "x" TInt (mkStrLit "hello" (loc 1 1)) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should fail" $ case result of
        Left _ -> True
        Right _ -> False
  ]

-- | Test cases for function declarations
testFuncDecls :: TestTree
testFuncDecls = testGroup "Function Declarations"
  [ testCase "Valid function declaration" $ do
      let stmt = mkFuncDecl "add" [("x", TInt), ("y", TInt)] TInt
            [mkReturn (mkBinOp "+" (mkVar "x" (loc 2 1)) (mkVar "y" (loc 2 3)) (loc 2 1)) (loc 2 1)]
            (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False

  , testCase "Return type mismatch" $ do
      let stmt = mkFuncDecl "add" [("x", TInt), ("y", TInt)] TString
            [mkReturn (mkBinOp "+" (mkVar "x" (loc 2 1)) (mkVar "y" (loc 2 3)) (loc 2 1)) (loc 2 1)]
            (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should fail" $ case result of
        Left _ -> True
        Right _ -> False
  ]

-- | Test cases for optional types
testOptionalTypes :: TestTree
testOptionalTypes = testGroup "Optional Types"
  [ testCase "Null assignment to optional" $ do
      let stmt = mkVarDecl "x" (mkOptional TInt) (mkNullLit (loc 1 1)) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False

  , testCase "Optional type compatibility" $ do
      let stmt = mkVarDecl "x" (mkOptional TInt) (mkIntLit 42 (loc 1 1)) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False
  ]

-- | Test cases for list types
testListTypes :: TestTree
testListTypes = testGroup "List Types"
  [ testCase "List type compatibility" $ do
      let stmt = mkVarDecl "xs" (mkList TInt) (mkList TInt) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False

  , testCase "List type mismatch" $ do
      let stmt = mkVarDecl "xs" (mkList TInt) (mkList TString) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should fail" $ case result of
        Left _ -> True
        Right _ -> False
  ]

-- | Test cases for map types
testMapTypes :: TestTree
testMapTypes = testGroup "Map Types"
  [ testCase "Map type compatibility" $ do
      let stmt = mkVarDecl "m" (mkMap TString TInt) (mkMap TString TInt) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False

  , testCase "Map type mismatch" $ do
      let stmt = mkVarDecl "m" (mkMap TString TInt) (mkMap TInt TString) (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should fail" $ case result of
        Left _ -> True
        Right _ -> False
  ]

-- | Test cases for if statements
testIfStmts :: TestTree
testIfStmts = testGroup "If Statements"
  [ testCase "Valid if statement" $ do
      let stmt = mkIf (mkBoolLit True (loc 1 1))
            [mkVarDecl "x" TInt (mkIntLit 42 (loc 2 1)) (loc 2 1)]
            (Just [mkVarDecl "x" TInt (mkIntLit 0 (loc 3 1)) (loc 3 1)])
            (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should succeed" $ case result of
        Right _ -> True
        Left _ -> False

  , testCase "Non-boolean condition" $ do
      let stmt = mkIf (mkIntLit 42 (loc 1 1))
            [mkVarDecl "x" TInt (mkIntLit 42 (loc 2 1)) (loc 2 1)]
            Nothing
            (loc 1 1)
      result <- typeCheckStmt stmt
      assertBool "Should fail" $ case result of
        Left _ -> True
        Right _ -> False
  ]

main :: IO ()
main = defaultMain $ testGroup "Type Checker Tests"
  [ testLiterals
  , testBinaryOps
  , testVarDecls
  , testFuncDecls
  , testOptionalTypes
  , testListTypes
  , testMapTypes
  , testIfStmts
  ] 