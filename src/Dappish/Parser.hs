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
       , pKnowOfLine
       ])

pKnowOfLine :: Parser MainLine
pKnowOfLine = try $
  KnowOfLine
    <$> (words "Let" *> pTheBox)
    <*> (words "know of" *> pTheBox)

pTheBox :: Parser BoxName
pTheBox = BoxName <$> (words "the" *> name)

pBoxDeclLine :: Parser MainLine
pBoxDeclLine = do
  words "Let there be an object called"
  theBoxName <- pTheBox
  theAlias <-
    optional (words ", also known as" *> quotation)
      <??> ", also known as \"...\""
  pure (BoxDeclLine theBoxName theAlias)

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> unpack ("«" <> b <> "»")

pVarDeclLine :: Parser MainLine
pVarDeclLine = do
  varName <-
    try $
      (words "Let the" *> pVarName <* words "be")
        <??> "Let the <object> <name> be <type>, ..."
  typeName <- pTypeName
  theAlias <-
    optional (words ", also known as" *> quotation)
      <??> ", also known as \"...\""
  words ", initially"
  varSource <- pVarSource
  pure (VarDeclLine varName typeName varSource theAlias)

quotation :: Parser Text
quotation =
  pack <$> (char '"' *> some (notChar '"') <* char '"')

pVarName :: Parser VarName
pVarName = do
  aBoxName <- BoxName <$> name
  VarName aBoxName <$> name

pTypeName :: Parser TypeName
pTypeName = choice
  [ words "a 27-decimal number" *> pure Ray
  , words "an 18-decimal number" *> pure Wad
  , words "a timestamp" *> pure Sec
  ]

pVarSource :: Parser VarSource
pVarSource = choice
  [ words "by parameter" *> pure ByParameter
  , words "set to" *> (Initially <$> pSimpleExpr)
  ]

pSimpleExpr :: Parser SimpleExpr
pSimpleExpr = choice
  [ words "one" *> pure One
  , words "now" *> pure Now
  , words "the" *> (The <$> pVarName)
  ]
