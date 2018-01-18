module Dappish.Solidify where

import Dappish.Prelude
import Dappish.AST
import Dappish.Grok

import Text.PrettyPrint hiding (text, (<>))
import qualified Text.PrettyPrint as PP

import qualified Data.Map.Strict as Map

txt :: Text -> Doc
txt = PP.text . unpack

solidify :: Grokked -> Text
solidify =
  pack . renderStyle (style { lineLength = 78 }) . convert

convert :: Grokked -> Doc
convert x = vcat
  [ txt "pragma solidity ^0.4.19;"
  , txt ""
  , vcat (map convertBoxDecl (Map.elems x))
  ]

convertBoxDecl :: BoxDecl -> Doc
convertBoxDecl x = vcat
  [ txt "contract" <+> txt (view (name . text) x) <+> txt "{"
  , nest 4 (vcat (map convertVarDecl (sortOn (view idx) (Map.elems (view vars x)))))
  , txt "}"
  , txt ""
  ]

convertVarDecl :: VarDecl -> Doc
convertVarDecl x =
  case view (name . alias) x of
    Nothing ->
      decl
    Just anAlias ->
      hsep [decl, txt ("// aka \"" <> anAlias <> "\"")]
  where
    decl =
      hsep
        [ convertTypeName (view typeName x)
        , txt (view (name . text) x) <> txt ";"
        ]

convertTypeName :: TypeName -> Doc
convertTypeName =
  \case
    Ray -> txt "uint128 /* ray */"
    Wad -> txt "uint128 /* wad */"
    Sec -> txt "uint64  /* sec */"
