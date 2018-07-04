module Test.Logic.RelationMap where

import Prelude

import Logic.RelationMap (addRelation, addRelations, addRelationsTo, addVariable, empty, getRelationsFrom, isRelation, mkRelationMap, newRelations, numVars, replaceRelations, rmRelation, rmRelations, rmVariable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

relationMapSpec :: Spec Unit
relationMapSpec =
  describe "relation map" do
    it "should create empty map" do
      numVars empty `shouldEqual` 0
    it "should create single map" do
      numVars (mkRelationMap 1) `shouldEqual` 1
    it "should create map with 2 vars" do
      numVars (mkRelationMap 2) `shouldEqual` 2
    it "should add a variable" do
      numVars (mkRelationMap 2) `shouldEqual` 2
      numVars (addVariable (mkRelationMap 2)) `shouldEqual` 3
    it "should default to no relation" do
      isRelation 0 1 (mkRelationMap 2) `shouldEqual` false
      isRelation 1 1 (mkRelationMap 2) `shouldEqual` false
    it "should add a relation" do
      (isRelation 0 1 $ mkRelationMap 2) `shouldEqual` false
      (isRelation 0 1 $ addRelation 0 1 $ mkRelationMap 2) `shouldEqual` true
    it "should add multiple relations" do
      let m = addRelations 0 [1,2] $ mkRelationMap 3
      isRelation 0 1 m `shouldEqual` true
      isRelation 0 2 m `shouldEqual` true
      isRelation 0 0 m `shouldEqual` false
    it "should remove a relation" do
      (isRelation 0 1 $ mkRelationMap 2) `shouldEqual` false
      (isRelation 0 1 $ addRelation 0 1 $ mkRelationMap 2) `shouldEqual` true
      (isRelation 0 1 $ rmRelation 0 1 $ addRelation 0 1 $ mkRelationMap 2) `shouldEqual` false
    it "should remove multiple relations" do
      let m = addRelations 0 [0,1,2] $ mkRelationMap 3
          m' = rmRelations 0 [1,2] m
      isRelation 0 0 m' `shouldEqual` true
      isRelation 0 1 m' `shouldEqual` false
      isRelation 0 2 m' `shouldEqual` false
    it "should blank a removed variable" do
      let m = addRelation 0 0 $ addRelation 1 0 $ addRelation 0 1 $ mkRelationMap 2
      isRelation 0 1 m `shouldEqual` true
      isRelation 1 0 m `shouldEqual` true
      isRelation 0 0 m `shouldEqual` true
      let m' = rmVariable 1 m
      isRelation 0 1 m' `shouldEqual` false
      isRelation 1 0 m' `shouldEqual` false
      isRelation 0 0 m' `shouldEqual` true
    it "should replace relations" do
      let m = addRelations 0 [1,2] $ mkRelationMap 3
          m' = replaceRelations 0 [0,1] m
      isRelation 0 0 m' `shouldEqual` true
      isRelation 0 1 m' `shouldEqual` true
      isRelation 0 2 m' `shouldEqual` false
    it "should retrieve relations" do
      let m = addRelations 0 [1,2] $ mkRelationMap 3
      getRelationsFrom 0 m `shouldEqual` [1,2]
      getRelationsFrom 1 m `shouldEqual` []
    it "should add relations to a var" do
      let m = addRelationsTo [0,2] 1 $ mkRelationMap 3
      isRelation 0 1 m `shouldEqual` true
      isRelation 1 1 m `shouldEqual` false
      isRelation 2 1 m `shouldEqual` true
      isRelation 1 2 m `shouldEqual` false
    it "should find new relations only" do
      let m = addRelation 0 1 $ mkRelationMap 3
          m' = addRelation 1 2 m
          m'' = newRelations m m'
      isRelation 0 1 m'' `shouldEqual` false
      isRelation 1 2 m'' `shouldEqual` true
