{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module ConfineSpec where

import Test.Hspec
import Confine
import Types
import Control.Monad


spec :: Spec
spec = do
  describe "setOccupant, getOccupant" $ do
    it "are a bit like inverses" $ do
      let game p1 p2 = setOccupant 0 p1 >> getOccupant p2

      forM_ [ ((i1, j1), (i2, j2)) | i1 <- [0..3], j1 <- [0..3], i2 <- [0..3], j2 <- [0..3] ] $
        \(p1, p2) -> run 3 ["me"] (game p1 p2) `shouldBe` Right (if p1 == p2 then Just 0 else Nothing)

    it "setOccupant throws exception if game is not in progress" $ do
      pending

    it "setOccupant throws exception if point is already occupied" $ do
      pending

    it "setOccupant throws exception if playerIndex is invalid" $ do
      pending

  describe "getEge" $ do
    it "works" pending

  describe "setEge" $ do
    it "works" pending

  describe "move" $ do
    it "works" pending
