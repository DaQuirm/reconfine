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

type Score = Int

-- | In 'Done', all 'Player's are listed.  The lists inside the list contain 'Player's with same
-- score.
data GameState priv = InProgress priv [Player] Int | Done [[(Player, Score)]]
  deriving (Eq, Ord, Show)

data GameError
  = InternalErrorOutOfBounds Point
  | EdgeAlreadySet Point Edge Player
  | PointAlreadyOccupied Point Player
  deriving (Eq, Ord, Show)

type MonadGame priv m {- :: Constraint -} = (MonadState (GameState priv) m, MonadError GameError m)
