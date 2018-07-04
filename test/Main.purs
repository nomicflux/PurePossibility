module Test.Main where

import Prelude

import Effect (Effect)
import Test.Logic.Properties (propertiesSpec)
import Test.Logic.RelationMap (relationMapSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  relationMapSpec
  propertiesSpec
