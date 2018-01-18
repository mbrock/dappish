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

data Constructor = Constructor
  { constructorBoxName :: BoxName
  , constructorParams :: [(VarName, TypeName)]
  , constructorAssignments :: [(VarName, TypeName, SimpleExpr)]
  } deriving (Show, Data)
makeFields ''Constructor

data BoxDecl = BoxDecl
  { boxDeclName :: BoxName
  , boxDeclIdx :: Idx BoxDecl
  , boxDeclVars :: Map VarName VarDecl
  } deriving (Show, Data)
makeFields ''BoxDecl

data Grokked = Grokked
  { grokkedConstructors :: Map BoxName Constructor
  , grokkedBoxDecls :: Map BoxName BoxDecl
  } deriving (Show, Data)
makeFields ''Grokked

emptyGrokked :: Grokked
emptyGrokked = Grokked
  { grokkedConstructors = mempty
  , grokkedBoxDecls = mempty
  }

data Failure
  = XDuplicateBox BoxName
  | XDuplicateVar VarName
  | XUnknownBox BoxName
  | XUnknownVar VarName
  | XCircularDefinition
  | XComplexInitialization BoxName VarName
  deriving (Show, Data)

gensym :: MonadState (x, Integer) m => m (Idx a)
gensym = Idx <$> (use _2 <* (_2 += 1))

grok :: DappSpec -> Either Failure Grokked
grok (DappSpec mainLines) =
  case runExcept (execStateT (mapM_ stage0 mainLines) s0) of
    Left e ->
      Left e
    Right (s1, _) ->
      runExcept (execStateT stage1 s1)
  where
    s0 = (emptyGrokked, 0)

    stage0 line = do
      (old, _) <- get
      case line of
        BoxDeclLine newBoxName -> do
          when
            (Map.member newBoxName (view boxDecls old))
            (throwError (XDuplicateBox newBoxName))
          next <- gensym
          _1 . boxDecls . at newBoxName .= Just BoxDecl
            { boxDeclName = newBoxName
            , boxDeclIdx = next
            , boxDeclVars = mempty
            }
        VarDeclLine newVarName newTypeName newVarInit -> do
          let theBoxName = view boxName newVarName
          case preview (boxDecls . ix theBoxName) old of
            Nothing ->
              throwError (XUnknownBox (view boxName newVarName))
            Just theBoxDecl -> do
              when
                (Map.member newVarName (view vars theBoxDecl))
                (throwError (XDuplicateVar newVarName))
              next <- gensym
              _1 . boxDecls . ix theBoxName . vars . at newVarName .= Just VarDecl
                { varDeclName = newVarName
                , varDeclIdx = next
                , varDeclTypeName = newTypeName
                , varDeclInit = newVarInit
                }

    stage1 :: StateT Grokked (Except Failure) ()
    stage1 = do
      -- We need to figure out how to build the constructor.
      -- The parameters are simply the variables which are "by parameter."
      -- The assignments are simple for "by parameter" or specific values.
      -- Variables given as other variable references should be checked.
      -- There can be no cycles there; in fact, a reference should be to
      -- a "by parameter" or specific value.
      (Map.elems <$> use boxDecls) >>= mapM_ inferConstructor

      where
        inferConstructor theBoxDecl = do
          let
            theBoxName = view name theBoxDecl
            theVars = view vars theBoxDecl
          theAssignments <- inferConstructorAssignments theBoxName theVars
          constructors . at theBoxName .= Just Constructor
            { constructorBoxName = theBoxName
            , constructorParams =
                map (\x -> (view name x, view typeName x))
                  (filter ((== ByParameter) . view init)
                    (sortOn (view idx) (Map.elems theVars)))
            , constructorAssignments =
                theAssignments
            }

        inferConstructorAssignments theBoxName theVars =
          mapM f
            (filter ((/= ByParameter) . view init)
              (sortOn (view idx) (Map.elems theVars)))
          where
            f x = do
              let theVarName = view name x
              case view init x of
                Initially (The refVarName) ->
                  case preview (ix refVarName) theVars of
                    Nothing ->
                      throwError (XUnknownVar refVarName)
                    Just y ->
                      case view init y of
                        ByParameter ->
                          pure (theVarName, view typeName x, The refVarName)
                        Initially (The _) ->
                          throwError $
                            XComplexInitialization theBoxName theVarName
                        Initially z ->
                          pure (theVarName, view typeName x, z)
                Initially One ->
                  pure (theVarName, view typeName x, One)
                Initially Now ->
                  pure (theVarName, view typeName x, Now)
