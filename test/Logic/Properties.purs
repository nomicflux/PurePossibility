module Test.Logic.Properties where

import Logic.Properties
import Prelude

import Logic.RelationMap (addRelation, isRelation, mkRelationMap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

propertiesSpec :: Spec Unit
propertiesSpec =
  describe "properties" do
    it "should give reflexive relations" do
      let m = mkRelationMap 3
          m' = reflexive m
      isRelation 0 0 m' `shouldEqual` true
      isRelation 1 1 m' `shouldEqual` true
      isRelation 2 2 m' `shouldEqual` true
    it "should give symmetric relations" do
      let m = addRelation 0 1 $ mkRelationMap 3
          m' = symmetric m
      isRelation 1 0 m `shouldEqual` false
      isRelation 1 0 m' `shouldEqual` true
    it "should give transitive relations" do
      let m = addRelation 1 2 $ addRelation 0 1 $ mkRelationMap 3
          m' = transitive m
      isRelation 0 2 m `shouldEqual` false
      isRelation 0 2 m' `shouldEqual` true
