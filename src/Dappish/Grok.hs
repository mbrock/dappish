{-# Language TemplateHaskell #-}

module Dappish.Grok where

import Dappish.Prelude
import Dappish.AST

import qualified Data.Map.Strict as Map

data VarDecl = VarDecl
  { varDeclName :: VarName
  , varDeclTypeName :: TypeName
  , varDeclInit :: VarSource
  } deriving (Show, Data)
makeFields ''VarDecl

data BoxDecl = BoxDecl
  { boxDeclName :: BoxName
  , boxDeclVars :: Map VarName VarDecl
  } deriving (Show, Data)
makeFields ''BoxDecl

data Constructor = Constructor
  { constructorBoxName :: BoxName
  , constructorParams :: [VarName]
  , constructorAssignments :: [(VarName, SimpleExpr)]
  } deriving (Show, Data)
makeFields ''Constructor

data Failure
  = XDuplicateBox BoxName
  | XDuplicateVar VarName
  | XUnknownBox BoxName
  | XUnknownVar VarName
  | XCircularDefinition
  | XComplexInitialization BoxName VarName
  deriving (Show, Data)

type Grokked = Map BoxName BoxDecl

grok :: DappSpec -> Either Failure (Map BoxName BoxDecl)
grok (DappSpec mainLines) =
  runExcept . flip execStateT s0 $ mapM_ go mainLines
  where
    s0 = mempty
    go line = do
      old <- get
      case line of
        BoxDeclLine newBoxName -> do
          when
            (Map.member newBoxName old)
            (throwError (XDuplicateBox newBoxName))
          at newBoxName .= Just BoxDecl
            { boxDeclName = newBoxName
            , boxDeclVars = mempty
            }
        VarDeclLine newVarName newTypeName newVarInit -> do
          let theBoxName = view boxName newVarName
          case Map.lookup theBoxName old of
            Nothing ->
              throwError (XUnknownBox (view boxName newVarName))
            Just theBoxDecl -> do
              when
                (Map.member newVarName (view vars theBoxDecl))
                (throwError (XDuplicateVar newVarName))
              ix theBoxName . vars . at newVarName .= Just VarDecl
                { varDeclName = newVarName
                , varDeclTypeName = newTypeName
                , varDeclInit = newVarInit
                }

-- We need to figure out how to build the constructor.
-- The parameters are simply the variables which are "by parameter."
-- The assignments are simple for "by parameter" or specific values.
-- Variables given as other variable references should be checked.
-- There can be no cycles there; in fact, a reference should be to
-- a "by parameter" or specific value.
