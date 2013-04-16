module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Data.Time.RFC2822Tests
import Data.Time.RFC3339Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
          rfc2822Tests
        , rfc3339Tests
        ]
