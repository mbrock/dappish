module Dappish.Prelude (module Dappish.Prelude, module X) where

import Prelude as X
  ( Read (..), Show (..), read
  , Eq (..), Ord (..)
  , Functor (..)
  , Applicative (..)
  , Monad (..)
  , Bool (..)
  , Maybe (..)
  , Either (..)
  , IO
  , String
  , Integer
  , (.)
  , ($)
  , flip
  , map
  , elem
  , print
  , putStrLn
  , error
  )

import Control.Applicative as X
import Control.Lens as X
import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.State as X
import Data.Data as X (Data)
import Data.Foldable as X (foldr, foldl', toList)
import Data.Monoid as X
import Data.Text as X (Text, pack, unpack, isSuffixOf)
import Data.Map.Strict as X (Map)
import Data.List as X (sort, sortBy, filter, concat)
import Data.Function as X (on)
import Data.Void as X
import Data.Char as X (toUpper)
import Data.Set as X (Set)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
