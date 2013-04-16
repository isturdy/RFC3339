{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module      : Data.Time.RFC2822
-- Copyright   : (c) 2011 Hugo Daniel Gomes
--
-- License     : BSD-style
-- Maintainer  : mr.hugo.gomes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for reading and displaying time in the format specified by
-- the RFC2822 <http://www.ietf.org/rfc/rfc2822.txt> section 3.3
--
-- Example of usage:
--
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO String
-- > showTime = getZonedTime >>= return . showRFC2822
-- >
-- > example1 = "Fri, 21 Nov 1997 09:55:06 -0600"
-- > example2 = "Tue, 15 Nov 1994 12:45:26 GMT"
-- > example3 = "Tue, 1 Jul 2003 10:52:37 +0200"
-- > example4 = "Thu, 13 Feb 1969 23:32:54 -0330"
-- > example5 = "Mon, 24 Nov 1997 14:22:01 -0800"
-- > example6 = "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330"
-- > example7 = "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)"
-- > example8 = "24 Nov 1997 14:22:01 -0800"
-- > examples = [example1,example2,example3,example4,example5,example6,example7,example8]
-- >
-- > readAll = map readRFC2822 examples

module Data.Time.RFC2822 (
    -- * Basic type class
    -- $basic
    RFC2822(showRFC2822, readRFC2822)
) where

import Control.Applicative
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

-- ----------------------------------------------------------------------------
-- The RFC2822 class definition

-- | This class is here to allow future support for other data types
-- if that becomes necessary
class RFC2822 a where
  showRFC2822 :: ZonedTime -> a
  readRFC2822 :: a -> Maybe ZonedTime
  formatRFC2822 :: [a]

instance RFC2822 String where
  showRFC2822 zt@(ZonedTime _ z) =
    formatTime defaultTimeLocale "%a, %e %b %Y %T" zt ++ printZone
    where
      timeZoneStr = timeZoneOffsetString z
      printZone = if timeZoneStr == timeZoneOffsetString utc
                    then " GMT"
                    else " " ++ timeZoneStr

  formatRFC2822 = [ "%a, %e %b %Y %T GMT"
                  , "%a, %e %b %Y %T %z"
                  , "%e %b %Y %T GMT"
                  , "%e %b %Y %T %z"
                  -- Support for hours:minutes
                  , "%a, %e %b %Y %R GMT"
                  , "%a, %e %b %Y %R %z"
                  , "%e %b %Y %R GMT"
                  , "%e %b %Y %R %z"
                  ]

  readRFC2822 t = foldr tryParse Nothing formatRFC2822
    where
      tryParse :: String -> Maybe ZonedTime -> Maybe ZonedTime
      tryParse fmt acc = acc <|> parseTime defaultTimeLocale fmt t

instance RFC2822 T.Text where
  showRFC2822 = T.pack . showRFC2822
  formatRFC2822 = fmap T.pack formatRFC2822
  readRFC2822 = readRFC2822 . T.unpack
