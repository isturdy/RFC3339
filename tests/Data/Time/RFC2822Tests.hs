module Data.Time.RFC2822Tests (rfc2822Tests) where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Time.RFC2822

rfc2822Tests = testGroup "Data.Text.RFC2822" [
  parserTest
  ]

parserTest = testCase "Parser test" $
             length (catMaybes (map readRFC2822 tests)) @?= (length tests)-3

tests :: [String]
tests = [
    "Fri, 21 Nov 1997 09:55:06 -0600"
    , "Tue, 15 Nov 1994 12:45:26 GMT"
    , "Tue, 1 Jul 2003 10:52:37 +0200"
    , "Thu, 13 Feb 1969 23:32:54 -0330"
    , "Mon, 24 Nov 1997 14:22:01 -0800"
    , "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330"
    , "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)" -- Fails
    , "24 Nov 1997 14:22:01 -0800"
    , "15 Nov 1994 12:45:26 GMT"
    , "Mon,24 Nov 1997 14:22:01 -0800"
    , "Thu,\t13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)"  -- Fails
    , "Thu, 13 Feb 1969 23:32 -0330 (Newfoundland Time)"  -- Fails
    ]
