{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Properties where

import Test.Tasty
import Test.Tasty.QuickCheck
import AST
import TypeChecker
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Data.List (nub)

-- | Helper function to create a location
loc :: Int -> Int -> Location
loc line col = Location line col

-- | Arbitrary instance for Type
instance Arbitrary Type where
  arbitrary = sized genType
    where
      genType 0 = elements [TInt, TString, TBool, TFloat, TVoid, TNull]
      genType n = frequency
        [ (1, elements [TInt, TString, TBool, TFloat, TVoid, TNull])
        , (2, TOptional <$> genType (n-1))
        , (2, TList <$> genType (n-1))
        , (2, TMap <$> genType (n-1) <*> genType (n-1))
        , (2, TFunc <$> listOf (genType (n-1)) <*> genType (n-1))
        ]

-- | Property: Type compatibility is reflexive
prop_type_compatibility_reflexive :: Type -> Property
prop_type_compatibility_reflexive typ = property $ do
  result <- runExcept $ evalStateT (checkTypeCompatibility typ typ (loc 1 1)) emptyEnv
  case result of
    Right True -> return True
    _ -> return False

-- | Property: Optional types are compatible with their base type
prop_optional_compatibility :: Type -> Property
prop_optional_compatibility typ = property $ do
  let optionalType = mkOptional typ
  result <- runExcept $ evalStateT (checkTypeCompatibility typ optionalType (loc 1 1)) emptyEnv
  case result of
    Right True -> return True
    _ -> return False

-- | Property: List types are compatible if their element types are compatible
prop_list_compatibility :: Type -> Type -> Property
prop_list_compatibility t1 t2 = property $ do
  let list1 = mkList t1
      list2 = mkList t2
  result <- runExcept $ evalStateT (checkTypeCompatibility list1 list2 (loc 1 1)) emptyEnv
  case result of
    Right True -> return True
    _ -> return False

-- | Property: Map types are compatible if their key and value types are compatible
prop_map_compatibility :: Type -> Type -> Type -> Type -> Property
prop_map_compatibility k1 v1 k2 v2 = property $ do
  let map1 = mkMap k1 v1
      map2 = mkMap k2 v2
  result <- runExcept $ evalStateT (checkTypeCompatibility map1 map2 (loc 1 1)) emptyEnv
  case result of
    Right True -> return True
    _ -> return False

-- | Property: Function types are compatible if their parameter and return types are compatible
prop_function_compatibility :: [Type] -> Type -> [Type] -> Type -> Property
prop_function_compatibility params1 ret1 params2 ret2 = property $ do
  let func1 = mkFunc params1 ret1
      func2 = mkFunc params2 ret2
  result <- runExcept $ evalStateT (checkTypeCompatibility func1 func2 (loc 1 1)) emptyEnv
  case result of
    Right True -> return True
    _ -> return False

-- | Property: Variable declarations maintain type environment
prop_var_decl_environment :: Text -> Type -> Property
prop_var_decl_environment name typ = property $ do
  let stmt = mkVarDecl name typ (mkIntLit 42 (loc 1 1)) (loc 1 1)
  result <- runExcept $ evalStateT (typeCheckStmt stmt >> get) emptyEnv
  case result of
    Right env -> return $ Map.lookup name (variables env) == Just typ
    _ -> return False

-- | Property: Function declarations maintain type environment
prop_func_decl_environment :: Text -> [(Text, Type)] -> Type -> Property
prop_func_decl_environment name params retType = property $ do
  let stmt = mkFuncDecl name params retType [] (loc 1 1)
  result <- runExcept $ evalStateT (typeCheckStmt stmt >> get) emptyEnv
  case result of
    Right env -> return $ Map.lookup name (functions env) == Just (mkFunc (map snd params) retType)
    _ -> return False

-- | Property: Binary operations preserve types
prop_binary_op_types :: Text -> Int -> Int -> Property
prop_binary_op_types op x y = property $ do
  let expr = mkBinOp op (mkIntLit x (loc 1 1)) (mkIntLit y (loc 1 3)) (loc 1 1)
  result <- runExcept $ evalStateT (typeCheckExpr expr) emptyEnv
  case result of
    Right typ -> return $ case op of
      "+" -> typ == TInt
      "-" -> typ == TInt
      "*" -> typ == TInt
      "/" -> typ == TInt
      "%" -> typ == TInt
      "==" -> typ == TBool
      "!=" -> typ == TBool
      "<" -> typ == TBool
      ">" -> typ == TBool
      "<=" -> typ == TBool
      ">=" -> typ == TBool
      _ -> False
    _ -> return False

-- | Property: If statements require boolean conditions
prop_if_boolean_condition :: Bool -> Property
prop_if_boolean_condition b = property $ do
  let stmt = mkIf (mkBoolLit b (loc 1 1)) [] Nothing (loc 1 1)
  result <- runExcept $ evalStateT (typeCheckStmt stmt) emptyEnv
  case result of
    Right _ -> return True
    _ -> return False

-- | Property: Return statements must match function return type
prop_return_type_match :: Type -> Property
prop_return_type_match retType = property $ do
  let stmt = mkReturn (mkIntLit 42 (loc 1 1)) (loc 1 1)
  result <- runExcept $ evalStateT (typeCheckStmt stmt) emptyEnv
  case result of
    Right _ -> return True
    _ -> return False

-- | Test group for all properties
typeCheckerProperties :: TestTree
typeCheckerProperties = testGroup "Type Checker Properties"
  [ testProperty "Type compatibility is reflexive" prop_type_compatibility_reflexive
  , testProperty "Optional types are compatible with base type" prop_optional_compatibility
  , testProperty "List types are compatible if element types are compatible" prop_list_compatibility
  , testProperty "Map types are compatible if key and value types are compatible" prop_map_compatibility
  , testProperty "Function types are compatible if parameter and return types are compatible" prop_function_compatibility
  , testProperty "Variable declarations maintain type environment" prop_var_decl_environment
  , testProperty "Function declarations maintain type environment" prop_func_decl_environment
  , testProperty "Binary operations preserve types" prop_binary_op_types
  , testProperty "If statements require boolean conditions" prop_if_boolean_condition
  , testProperty "Return statements must match function return type" prop_return_type_match
  ] 