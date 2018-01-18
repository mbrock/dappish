{-# Language TemplateHaskell #-}

module Dappish.Parser
  ( BoxName (..)
  , VarName (..)
  , VarSource (..)
  , SimpleExpr (..)
  , TypeName (..)
  , MainLine (..)
  , DappSpec (..)
  , parseFromFile
  , text
  , boxName
  ) where

import Dappish.Prelude
import Dappish.AST

import Text.Parser.LookAhead
import Text.Trifecta hiding (parseFromFile, text)
import qualified Data.Text as Text
import qualified Text.Trifecta as Trifecta

parseFromFile :: MonadIO m => String -> m (Maybe DappSpec)
parseFromFile = Trifecta.parseFromFile pDappSpec

words :: Text -> Parser ()
words s =
  (try . mapM_ (>> fin) . map Trifecta.text . Text.words $ s)
    <?> unpack ("\"" <> s <> " ...\"")

fin :: Parser ()
fin = (space >> spaces) <|> void (lookAhead (oneOf ".,"))

name :: Parser Text
name = pack <$> some letter <* fin

period :: Parser ()
period = char '.' >> space >> spaces

sentence :: Parser a -> Parser a
sentence x = x <* period

pDappSpec :: Parser DappSpec
pDappSpec = DappSpec <$> some pMainLine

pMainLine :: Parser MainLine
pMainLine =
  sentence
    (choice
       [ pBoxDeclLine
       , pVarDeclLine
       ])

pBoxDeclLine :: Parser MainLine
pBoxDeclLine =
  BoxDeclLine <$> do
    words "There is an object called the"
    BoxName <$> name

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> unpack ("«" <> b <> "»")

pVarDeclLine :: Parser MainLine
pVarDeclLine = do
  varName <-
    try $
      (words "The variable" *> pVarName <* words "is a")
        <??> "The variable <object> <name> is a <type>, ..."
  typeName <- pTypeName
  words ","
  varSource <-
    choice
      [ words "by parameter" *> pure ByParameter
      , words "initially" *> (Initially <$> pSimpleExpr)
      ]
  theAlias <-
    optional (words ", also known as" *> quotation)
      <??> ", also known as \"...\""
  let theVarName = varName { varNameAlias = theAlias }
  pure (VarDeclLine theVarName typeName varSource)

quotation :: Parser Text
quotation =
  pack <$> (char '"' *> some (notChar '"') <* char '"')

pVarName :: Parser VarName
pVarName = do
  aBoxName <- BoxName <$> name
  VarName aBoxName <$> name <*> pure Nothing

pTypeName :: Parser TypeName
pTypeName = choice
  [ words "ray" *> pure Ray
  , words "wad" *> pure Wad
  , words "sec" *> pure Sec
  ]

pSimpleExpr :: Parser SimpleExpr
pSimpleExpr = choice
  [ words "one" *> pure One
  , words "now" *> pure Now
  , words "the" *> (The <$> pVarName)
  ]
