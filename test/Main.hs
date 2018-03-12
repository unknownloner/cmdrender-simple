module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec

conf :: Config
conf =
  defaultConfig

main :: IO ()
main = hspecWith conf Spec.spec
