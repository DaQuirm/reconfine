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

import Control.Lens
import Control.Monad
import Data.Maybe
import Test.Hspec

import Confine
import Confine.Internal
import Types


spec :: Spec
spec = do
  describe "setOccupant, getOccupant" $ do
    it "are a bit like inverses" $ do
      forM_ [ ((i1, j1), (i2, j2)) | i1 <- [0..3], j1 <- [0..3], i2 <- [0..3], j2 <- [0..3] ] $
        \(p1, p2) -> run 3 ["me"] (setOccupant 0 p1 >> getOccupant p2)
                       `shouldBe` Right (if p1 == p2 then Just 0 else Nothing)

    describe "setOccupant" $ do
      it "throws exception if game is not in progress" $ do
        let st = initialGameState 3 ["me"] & whatsNext .~ Right undefined
        run' st (setOccupant 0 (0, 0))
          `shouldBe` Left DoNotMoveOnFinishedGame

      it "throws exception if point is already occupied" $ do
        let player = 0
            point  = (1, 2)
        run 3 ["me"] (setOccupant player point >> setOccupant player point)
          `shouldBe` Left (PointAlreadyOccupied player point)

      it "throws exception if playerIndex is invalid" $ do
        let player = 1
            point  = (1, 2)
        run 3 ["me"] (setOccupant player point >> setOccupant player point)
          `shouldBe` Left (NoSuchPlayer player)

    describe "getOccupant" $ do
      it "works even if game is complete" $ do
        let st = initialGameState 3 ["me"] & whatsNext .~ Right undefined
        (isNothing <$> run' st (getOccupant (0, 0)))
          `shouldBe` Right True

      it "returns Just if point is occupied" $ do
        let player = 0
            point  = (1, 2)
        run 3 ["me"] (setOccupant player point >> getOccupant point)
          `shouldBe` Right (Just player)

      it "returns Nothing if point is not occupied" $ do
        let player = 0
            point1 = (1, 2)
            point2 = (2, 2)
        run 3 ["me"] (setOccupant player point1 >> getOccupant point2)
          `shouldBe` Right Nothing

  describe "getEge" $ do
    it "works" pending

  describe "setEge" $ do
    it "works" pending

  describe "move" $ do
    it "works" pending
