{-# Language TemplateHaskell #-}

module Dappish.Grok where

import Dappish.Prelude
import Dappish.AST

import qualified Data.Map.Strict as Map

newtype Idx a = Idx { idxIndex :: Integer }
  deriving (Ord, Eq, Show, Data)
makeFields ''Idx

data VarDecl = VarDecl
  { varDeclName :: VarName
  , varDeclIdx :: Idx VarDecl
  , varDeclTypeName :: TypeName
  , varDeclInit :: VarSource
  } deriving (Show, Data)
makeFields ''VarDecl

data BoxDecl = BoxDecl
  { boxDeclName :: BoxName
  , boxDeclIdx :: Idx BoxDecl
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

gensym :: MonadState (x, Integer) m => m (Idx a)
gensym = Idx <$> (use _2 <* (_2 += 1))

grok :: DappSpec -> Either Failure (Map BoxName BoxDecl)
grok (DappSpec mainLines) =
  runExcept . fmap (view _1) . flip execStateT s0 $ mapM_ go mainLines
  where
    s0 = (mempty, 0)
    go line = do
      (old, _) <- get
      case line of
        BoxDeclLine newBoxName -> do
          when
            (Map.member newBoxName old)
            (throwError (XDuplicateBox newBoxName))
          next <- gensym
          _1 . at newBoxName .= Just BoxDecl
            { boxDeclName = newBoxName
            , boxDeclIdx = next
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
              next <- gensym
              _1 . ix theBoxName . vars . at newVarName .= Just VarDecl
                { varDeclName = newVarName
                , varDeclIdx = next
                , varDeclTypeName = newTypeName
                , varDeclInit = newVarInit
                }

-- We need to figure out how to build the constructor.
-- The parameters are simply the variables which are "by parameter."
-- The assignments are simple for "by parameter" or specific values.
-- Variables given as other variable references should be checked.
-- There can be no cycles there; in fact, a reference should be to
-- a "by parameter" or specific value.
