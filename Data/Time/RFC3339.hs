{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module      : Data.Time.RFC3339
-- Copyright   : (c) 2011 Hugo Daniel Gomes
--
-- License     : BSD-style
-- Maintainer  : mr.hugo.gomes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for reading and displaying time in the format specified by
-- the RFC3339 <http://www.ietf.org/rfc/rfc3339.txt>
--
-- Example of usage:
--
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO String
-- > showTime = getZonedTime >>= return . showRFC3339
-- >
-- > example1 = "1985-04-12T23:20:50.52Z"
-- > example2 = "1996-12-19T16:39:57-08:00"
-- > example3 = "1990-12-31T23:59:60Z"
-- > example4 = "1990-12-31T15:59:60-08:00"
-- > example5 = "1937-01-01T12:00:27.87+00:20"
-- > examples = [example1,example2,example3,example4,example5]
-- >
-- > readAll = map readRFC3339 examples

module Data.Time.RFC3339 (
    -- * Basic type class
    -- $basic
    RFC3339(showRFC3339, readRFC3339)
) where

import Control.Applicative
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

-- ----------------------------------------------------------------------------
-- The RFC3339 class definition

-- | This class is here to allow future support for other data types
-- if that becomes necessary
class RFC3339 a where
  showRFC3339 :: ZonedTime -> a
  readRFC3339 :: a -> Maybe ZonedTime
  formatRFC3339 :: [a]

instance RFC3339 String where
  showRFC3339 zt@(ZonedTime _ z) =
    formatTime defaultTimeLocale "%FT%T" zt ++ printZone
    where
      timeZoneStr = timeZoneOffsetString z
      printZone = if timeZoneStr == timeZoneOffsetString utc
                    then "Z"
                    else take 3 timeZoneStr ++ ":" ++ drop 3 timeZoneStr
  formatRFC3339 = [ "%FT%TZ"
                  , "%FT%T%z"
                  , "%FT%T%Q%z"
                  , "%FT%T%QZ"
                  ]
  readRFC3339 t = foldr tryParse Nothing formatRFC3339
    where
      tryParse :: String -> Maybe ZonedTime -> Maybe ZonedTime
      tryParse fmt acc = acc <|> parseTime defaultTimeLocale fmt t

instance RFC3339 T.Text where
  showRFC3339 = T.pack . showRFC3339
  formatRFC3339 = fmap T.pack formatRFC3339
  readRFC3339 = readRFC3339 . T.unpack
