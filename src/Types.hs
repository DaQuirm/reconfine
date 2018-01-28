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

module Types where

import Control.Monad.State
import Control.Monad.Except

type X = Int
type Y = Int
type Point = (X, Y)

data Edge = TopEdge | BottomEdge | LeftEdge | RightEdge
  deriving (Eq, Ord, Show)

type Player = String
type PlayerIndex = Int

type BoardSize = Int

type Score = Int

-- | All 'Player's are listed.  The lists inside the list contain 'Player's with same score.
type GameResult = [[(Player, Score)]]

data GameError
  = PointOutOfBounds Point
  | NoSuchPlayer PlayerIndex
  | EdgeAlreadyBuilt PlayerIndex Point Edge
  | PointAlreadyOccupied PlayerIndex Point
  | DoNotMoveOnFinishedGame
  deriving (Eq, Ord, Show)


class MonadGame m where
  getOccupant :: Point -> m (Maybe PlayerIndex)
  setOccupant :: PlayerIndex -> Point -> m ()
  getEdge     :: Point -> Edge -> m Bool
  setEdge     :: Point -> Edge -> m ()  -- you can only switch once from False to True.

  getPlayer     :: PlayerIndex -> m Player
  currentPlayer :: m PlayerIndex
  nextPlayer    :: m ()
