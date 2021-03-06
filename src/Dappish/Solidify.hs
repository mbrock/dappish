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
  , txt "import \"ds-thing\";"
  , txt ""
  , vcat (map (convertBoxDecl x) (sortOn (view idx) (Map.elems (view boxDecls x))))
  ]

convertBoxDecl :: Grokked -> BoxDecl -> Doc
convertBoxDecl grokked x = vcat
  [ hsep
      [ txt "contract"
      , txt (capitalize (view (name . text) x))
          <> case view alias x of
               Nothing -> mempty
               Just anAlias -> txt (" /* " <> anAlias <> " */")
      , "is DSThing"
      , txt "{"
      ]
  , nest 4 (vcat (map convertKnowsOf (sortOn (view idx) (toList (view knows x)))))
  , nest 4 (vcat (map convertVarDecl (sortOn (view idx) (Map.elems (view vars x)))))
  , nest 4 (convertConstructor grokked x)
  , txt "}"
  , txt ""
  ]

capitalize :: Text -> Text
capitalize = over _head toUpper

convertConstructor :: Grokked -> BoxDecl -> Doc
convertConstructor g aBoxDecl =
  case preview (constructors . ix (view name aBoxDecl)) g of
    Nothing -> mempty
    Just x -> vcat
      [ txt ""
      , hsep
          [ txt "function"
          , txt (capitalize (view (boxName . text) x))
          , parens (commatized (map convertParam (view params x)))
          , "{"
          ]
      , nest 4 (vcat (map assignParam (view params x)))
      , nest 4 (vcat (map convertAssignment (view assignments x)))
      , txt "}"
      ]

  where
    convertParam (x, y) = hsep
      [ convertTypeName y
      , txt ("_" <> view text x)
      ]
    assignParam (x, _) = hsep
      [ txt (view text x)
      , "="
      , txt ("_" <> view text x <> ";")
      ]
    convertAssignment (theVarName, theTypeName, theSimpleExpr) =
      case theSimpleExpr of
        Zero ->
          hsep [ "//", txt (view text theVarName), "is zero" ]
        _ ->
          hsep
            [ txt (view text theVarName)
            , "="
            , convertSimpleExpr theTypeName theSimpleExpr <> txt ";"
            ]

convertSimpleExpr :: TypeName -> SimpleExpr -> Doc
convertSimpleExpr theTypeName = \case
  Zero ->
    case theTypeName of
      Ray -> txt "0"
      Wad -> txt "0"
      Sec -> txt "0"
      _ -> error "weird"
  One ->
    case theTypeName of
      Ray -> txt "ONE_D27"
      Wad -> txt "ONE_D18"
      _ -> error "weird"
  Now ->
    case theTypeName of
      Sec -> txt "era()"
      _ -> error "weird"
  The x ->
    txt (view text x)

commatized :: [Doc] -> Doc
commatized = hsep . punctuate (txt ",")

convertKnowsOf :: BoxDecl -> Doc
convertKnowsOf x =
  case view alias x of
    Nothing ->
      decl
    Just anAlias ->
      hsep [decl, txt ("// " <> anAlias)]
  where
    decl =
      hsep
        [ convertTypeName (TheBox (view name x))
        , txt (view (name . text) x) <> txt ";"
        ]

convertVarDecl :: VarDecl -> Doc
convertVarDecl x =
  case view alias x of
    Nothing ->
      decl
    Just anAlias ->
      hsep [decl, txt ("// " <> anAlias)]
  where
    decl =
      hsep
        [ convertTypeName (view typeName x)
        , txt (view (name . text) x) <> txt ";"
        ]

convertTypeName :: TypeName -> Doc
convertTypeName =
  \case
    Ray -> txt "uint /* fixed-d27 */"
    Wad -> txt "uint /* fixed-d18 */"
    Sec -> txt "uint /* timestamp */"
    TheBox x -> txt (capitalize (view text x))
