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

module Confine where

import Control.Monad.State
import Control.Monad.Error

type X = Int
type Y = Int
type Point = (X, Y)

type Player = String

type Score = Int

-- | in 'Done', all 'Player's are listed.  list inside list contains 'Player's with same score.
data GameState = InProgress | Done [[(Player, Score)]]
  deriving (Eq, Ord, Show)

emptyState :: GameState
emptyState = InProgress

data GameError = InternalOutOfBounds Point | EdgeAlreadySet Point Edge
  deriving (Eq, Ord, Show)

type Game m {- :: Constraint -} = (MonadState GameState m, MonadError GameError m)


getOccupant :: Game m => Point -> m Player
getOccupant = _

setOccupant :: Game m => Player -> Point -> m ()
setOccupant = _


data Edge = TopEdge | BottomEdge | LeftEdge | RightEdge
  deriving (Eq, Ord, Show)

getEdge :: Game m => Point -> Edge -> m Bool
getEdge = _

-- | you can only switch once from False to True.
setEdge :: Game m => Point -> Edge -> m ()
setEdge = _


-- | 'setEdge', then potentially 'setOccupant' of (one of) the two affected cells.
move :: Game m => Player -> Point -> Edge -> m GameState
move = _


run :: (MonadIO m, Game m) => GameState -> Game m -> IO (Either GameError GameState)
run = _
