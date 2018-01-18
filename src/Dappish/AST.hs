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
  }
  deriving (Ord, Eq, Show, Data)

makeFields ''VarName

data VarSource = ByParameter | Initially SimpleExpr
  deriving (Eq, Ord, Show, Data)

data SimpleExpr = One | Now | The VarName
  deriving (Eq, Ord, Show, Data)

data TypeName = Ray | Wad | Sec | TheBox BoxName
  deriving (Eq, Ord, Show, Data)

data MainLine
  = BoxDeclLine BoxName (Maybe Text)
  | VarDeclLine VarName TypeName VarSource (Maybe Text)
  | KnowOfLine BoxName BoxName
  deriving (Eq, Show, Data)

data DappSpec = DappSpec [MainLine]
  deriving (Eq, Show, Data)

makePrisms ''MainLine
