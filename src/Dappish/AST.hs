{-# Language TemplateHaskell #-}

module Dappish.AST where

import Dappish.Prelude

data BoxName = BoxName
  { boxNameText :: Text
  }
  deriving (Ord, Eq, Show, Data)

makeFields ''BoxName

data VarName = VarName
  { varNameBoxName :: BoxName
  , varNameText :: Text
  , varNameAlias :: Maybe Text
  }
  deriving (Ord, Eq, Show, Data)

makeFields ''VarName

data VarSource = ByParameter | Initially SimpleExpr
  deriving (Eq, Show, Data)

data SimpleExpr = One | Now | The VarName
  deriving (Eq, Show, Data)

data TypeName = Ray | Wad | Sec
  deriving (Eq, Show, Data)

data MainLine
  = BoxDeclLine BoxName
  | VarDeclLine VarName TypeName VarSource
  deriving (Eq, Show, Data)

data DappSpec = DappSpec [MainLine]
  deriving (Eq, Show, Data)

makePrisms ''MainLine
