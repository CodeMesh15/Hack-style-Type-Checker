{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module AST where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T

-- | Location information for error reporting
data Location = Location
  { line :: Int
  , column :: Int
  } deriving (Eq, Show, Generic)

-- | Types supported by the language
data Type
  = TInt
  | TString
  | TBool
  | TFloat
  | TVoid
  | TNull
  | TOptional Type
  | TList Type
  | TMap Type Type
  | TFunc [Type] Type
  deriving (Eq, Show, Generic)

-- | Literal values
data Literal
  = LInt Int
  | LStr Text
  | LBool Bool
  | LNull
  deriving (Eq, Show, Generic)

-- | Expressions
data Expr
  = ELit Literal Location
  | EVar Text Location
  | EBinOp Text Expr Expr Location
  | ECall Expr [Expr] Location
  deriving (Eq, Show, Generic)

-- | Statements
data Stmt
  = SVarDecl Text Type Expr Location
  | SFuncDecl Text [(Text, Type)] Type [Stmt] Location
  | SReturn Expr Location
  | SIf Expr [Stmt] (Maybe [Stmt]) Location
  deriving (Eq, Show, Generic)

-- | Program is a list of statements
data Program = Program
  { statements :: [Stmt]
  } deriving (Eq, Show, Generic)

-- | Type error with location information
data TypeError = TypeError
  { errorMessage :: Text
  , errorLocation :: Location
  } deriving (Eq, Show, Generic)

-- | Type checking result
data TypeCheckResult = TypeCheckResult
  { resultType :: Type
  , typeErrors :: [TypeError]
  } deriving (Eq, Show, Generic)

-- | Type environment using immutable maps
data TypeEnv = TypeEnv
  { variables :: Map Text Type
  , functions :: Map Text (TFunc [Type] Type)
  } deriving (Eq, Show, Generic)

-- | Helper functions for creating AST nodes
mkIntLit :: Int -> Location -> Expr
mkIntLit n loc = ELit (LInt n) loc

mkStrLit :: Text -> Location -> Expr
mkStrLit s loc = ELit (LStr s) loc

mkBoolLit :: Bool -> Location -> Expr
mkBoolLit b loc = ELit (LBool b) loc

mkNullLit :: Location -> Expr
mkNullLit loc = ELit LNull loc

mkVar :: Text -> Location -> Expr
mkVar name loc = EVar name loc

mkBinOp :: Text -> Expr -> Expr -> Location -> Expr
mkBinOp op left right loc = EBinOp op left right loc

mkCall :: Expr -> [Expr] -> Location -> Expr
mkCall fn args loc = ECall fn args loc

-- | Helper functions for creating statements
mkVarDecl :: Text -> Type -> Expr -> Location -> Stmt
mkVarDecl name typ init loc = SVarDecl name typ init loc

mkFuncDecl :: Text -> [(Text, Type)] -> Type -> [Stmt] -> Location -> Stmt
mkFuncDecl name params retType body loc = SFuncDecl name params retType body loc

mkReturn :: Expr -> Location -> Stmt
mkReturn expr loc = SReturn expr loc

mkIf :: Expr -> [Stmt] -> Maybe [Stmt] -> Location -> Stmt
mkIf cond thenBranch elseBranch loc = SIf cond thenBranch elseBranch loc

-- | Helper functions for creating types
mkOptional :: Type -> Type
mkOptional = TOptional

mkList :: Type -> Type
mkList = TList

mkMap :: Type -> Type -> Type
mkMap = TMap

mkFunc :: [Type] -> Type -> Type
mkFunc = TFunc

-- | Helper functions for creating types
mkOptional :: Type -> Type
mkOptional = TOptional

mkList :: Type -> Type
mkList = TList

mkMap :: Type -> Type -> Type
mkMap = TMap

mkFunc :: [Type] -> Type -> Type
mkFunc = TFunc 