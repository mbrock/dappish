module Dappish.Prelude (module X) where

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
  , (.)
  , ($)
  , flip
  , map
  , elem
  , print
  , putStrLn
  )

import Control.Applicative as X
import Control.Lens as X
import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.State as X
import Data.Data as X (Data)
import Data.Foldable as X (foldr, foldl')
import Data.Monoid as X
import Data.Text as X (Text, pack, unpack)
import Data.Map.Strict as X (Map)
