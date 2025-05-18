{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TypeChecker where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except

-- | Type checking monad
type TypeCheckM = StateT TypeEnv (Except [TypeError])

-- | Create an empty type environment
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty Map.empty

-- | Add a variable to the type environment
addVariable :: Text -> Type -> TypeEnv -> TypeEnv
addVariable name typ env = env { variables = Map.insert name typ (variables env) }

-- | Add a function to the type environment
addFunction :: Text -> Type -> TypeEnv -> TypeEnv
addFunction name typ env = env { functions = Map.insert name typ (functions env) }

-- | Check if a type is compatible with another type
checkTypeCompatibility :: Type -> Type -> Location -> TypeCheckM Bool
checkTypeCompatibility actual expected loc = do
  case (actual, expected) of
    -- Null is compatible with optional types
    (TNull, TOptional _) -> return True
    
    -- Optional types are compatible if their base types are compatible
    (TOptional t1, TOptional t2) -> checkTypeCompatibility t1 t2 loc
    
    -- List types are compatible if their element types are compatible
    (TList t1, TList t2) -> checkTypeCompatibility t1 t2 loc
    
    -- Map types are compatible if their key and value types are compatible
    (TMap k1 v1, TMap k2 v2) -> do
      kCompat <- checkTypeCompatibility k1 k2 loc
      vCompat <- checkTypeCompatibility v1 v2 loc
      return $ kCompat && vCompat
    
    -- Function types are compatible if their parameter and return types are compatible
    (TFunc params1 ret1, TFunc params2 ret2) -> do
      if length params1 /= length params2
        then throwError [TypeError "Function parameter count mismatch" loc]
        else do
          paramCompat <- zipWithM checkTypeCompatibility params1 params2
          retCompat <- checkTypeCompatibility ret1 ret2 loc
          return $ all id paramCompat && retCompat
    
    -- Primitive types must match exactly
    (t1, t2) -> return $ t1 == t2

-- | Type check an expression
typeCheckExpr :: Expr -> TypeCheckM Type
typeCheckExpr expr = case expr of
  ELit lit loc -> case lit of
    LInt _ -> return TInt
    LStr _ -> return TString
    LBool _ -> return TBool
    LNull -> return TNull
  
  EVar name loc -> do
    env <- get
    case Map.lookup name (variables env) of
      Just typ -> return typ
      Nothing -> throwError [TypeError ("Undefined variable: " <> name) loc]
  
  EBinOp op left right loc -> do
    leftType <- typeCheckExpr left
    rightType <- typeCheckExpr right
    
    case op of
      -- Arithmetic operations require number operands
      "+" | isNumeric leftType && isNumeric rightType -> return TInt
      "-" | isNumeric leftType && isNumeric rightType -> return TInt
      "*" | isNumeric leftType && isNumeric rightType -> return TInt
      "/" | isNumeric leftType && isNumeric rightType -> return TInt
      "%" | isNumeric leftType && isNumeric rightType -> return TInt
      
      -- String concatenation
      "+" | isString leftType && isString rightType -> return TString
      
      -- Comparison operations
      "==" -> checkComparison leftType rightType loc >> return TBool
      "!=" -> checkComparison leftType rightType loc >> return TBool
      "<" -> checkComparison leftType rightType loc >> return TBool
      ">" -> checkComparison leftType rightType loc >> return TBool
      "<=" -> checkComparison leftType rightType loc >> return TBool
      ">=" -> checkComparison leftType rightType loc >> return TBool
      
      -- Invalid operation
      _ -> throwError [TypeError ("Invalid binary operation: " <> op) loc]
  
  ECall fn args loc -> do
    fnType <- typeCheckExpr fn
    case fnType of
      TFunc paramTypes retType -> do
        if length args /= length paramTypes
          then throwError [TypeError "Function call argument count mismatch" loc]
          else do
            argTypes <- mapM typeCheckExpr args
            zipWithM_ checkTypeCompatibility argTypes paramTypes
            return retType
      _ -> throwError [TypeError "Cannot call non-function expression" loc]

-- | Type check a statement
typeCheckStmt :: Stmt -> TypeCheckM ()
typeCheckStmt stmt = case stmt of
  SVarDecl name typ init loc -> do
    initType <- typeCheckExpr init
    compat <- checkTypeCompatibility initType typ loc
    unless compat $ throwError [TypeError "Type mismatch in variable declaration" loc]
    modify $ addVariable name typ
  
  SFuncDecl name params retType body loc -> do
    -- Create new environment with parameters
    env <- get
    let paramEnv = foldr (\(name, typ) env -> addVariable name typ env) env params
    
    -- Type check function body
    put paramEnv
    mapM_ typeCheckStmt body
    
    -- Check return type compatibility
    returnStmts <- filterM isReturnStmt body
    forM_ returnStmts $ \retStmt -> case retStmt of
      SReturn expr _ -> do
        exprType <- typeCheckExpr expr
        compat <- checkTypeCompatibility exprType retType loc
        unless compat $ throwError [TypeError "Return type mismatch" loc]
      _ -> return ()
    
    -- Add function to environment
    let funcType = TFunc (map snd params) retType
    put env
    modify $ addFunction name funcType
  
  SReturn expr loc -> do
    _ <- typeCheckExpr expr
    return ()
  
  SIf cond thenBranch elseBranch loc -> do
    condType <- typeCheckExpr cond
    unless (condType == TBool) $
      throwError [TypeError "Condition must be a boolean expression" loc]
    
    mapM_ typeCheckStmt thenBranch
    maybe (return ()) (mapM_ typeCheckStmt) elseBranch

-- | Type check a program
typeCheckProgram :: Program -> Either [TypeError] ()
typeCheckProgram Program{..} = runExcept $ evalStateT (mapM_ typeCheckStmt statements) emptyEnv

-- | Helper functions
isNumeric :: Type -> Bool
isNumeric TInt = True
isNumeric TFloat = True
isNumeric _ = False

isString :: Type -> Bool
isString TString = True
isString _ = False

checkComparison :: Type -> Type -> Location -> TypeCheckM ()
checkComparison t1 t2 loc = do
  compat <- checkTypeCompatibility t1 t2 loc
  unless compat $ throwError [TypeError "Cannot compare values of different types" loc]

isReturnStmt :: Stmt -> TypeCheckM Bool
isReturnStmt (SReturn _ _) = return True
isReturnStmt _ = return False 