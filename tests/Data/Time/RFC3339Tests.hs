module Data.Time.RFC3339Tests (rfc3339Tests) where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Time.RFC3339

rfc3339Tests = testGroup "Data.Text.RFC3339" [
  parserTest
  ]

parserTest = testCase "Parser test" $
             length (catMaybes (map readRFC3339 tests)) @?= length tests

tests :: [String]
tests = [
    "1985-04-12T23:20:50.52Z"
    , "1996-12-19T16:39:57-08:00"
    , "1990-12-31T23:59:60Z"
    , "1990-12-31T15:59:60-08:00"
    , "1937-01-01T12:00:27.87+00:20"
    ]
