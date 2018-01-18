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

pluralName :: Parser Text
pluralName = try $ do
  s <- pack <$> some letter <* fin
  unless ("s" `isSuffixOf` s)
    (raiseErr (failed "expected a plural noun"))
  pure (Text.dropEnd 1 s)

period :: Parser ()
period = char '.' >> space >> spaces

sentence :: Parser a -> Parser a
sentence x = x <* period

pDappSpec :: Parser DappSpec
pDappSpec = DappSpec <$> some pMainLine

pMainLine :: Parser MainLine
pMainLine = do
  void . optional $ do
    words "## "
    void (some (Trifecta.noneOf "\n") >> newline >> newline)
  sentence
    (choice
       [ pBoxDeclLine
       , pClassDeclLine
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

pTheClass :: Parser BoxName
pTheClass = BoxName <$> (words "the" *> pluralName)

pBoxDeclLine :: Parser MainLine
pBoxDeclLine = do
  words "Let there be an object called"
  theBoxName <- pTheBox
  theAlias <-
    optional (words ", also known as" *> quotation)
      <??> ", also known as _some alias_"
  pure (BoxDeclLine theBoxName theAlias Singleton)

pClassDeclLine :: Parser MainLine
pClassDeclLine = do
  words "Let there be a class of objects called"
  theClassName <- pTheClass
  theAlias <-
    optional (words ", also known as" *> quotation)
      <??> ", also known as _some alias_"
  pure (BoxDeclLine theClassName theAlias Multitude)

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
      <??> ", also known as _some alias_"
  words ", initially"
  varSource <- pVarSource
  pure (VarDeclLine varName typeName varSource theAlias)

quotation :: Parser Text
quotation =
  pack <$> (char '_' *> some (notChar '_') <* char '_')

pVarName :: Parser VarName
pVarName = do
  aBoxName <- BoxName <$> name
  VarName aBoxName <$> name

pTypeName :: Parser TypeName
pTypeName = choice
  [ words "a precise rate" *> pure Ray
  , words "a token quantity" *> pure Wad
  , words "a timestamp" *> pure Sec
  ]

pVarSource :: Parser VarSource
pVarSource = choice
  [ words "by parameter" *> pure ByParameter
  , words "set to" *> (Initially <$> pSimpleExpr)
  ]

pSimpleExpr :: Parser SimpleExpr
pSimpleExpr = choice
  [ words "zero" *> pure Zero
  , words "one" *> pure One
  , words "now" *> pure Now
  , words "the" *> (The <$> pVarName)
  ]
