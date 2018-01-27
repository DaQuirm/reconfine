{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
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

module Confine.Internal where

import Control.Exception (assert)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Monoid
import Data.Set as Set

import Types


type Game = ExceptT GameError (State GameState)

data GameState
  = InProgress
      { _boardsize :: BoardSize
      , _edges     :: Set Vector
      , _occupants :: Map Point PlayerIndex
      , _players   :: [Player]
      , _currentPl :: PlayerIndex
      }
  | Done GameResult
  deriving (Eq, Ord, Show)

type Vector = (Point, VectorDir)  -- (slight abuse of linear algebra or haskell terminology)

data VectorDir = VectorDown | VectorRight
  deriving (Eq, Ord, Show)

$(makeLenses ''GameState)


-- TODO: shuffle players in random order (not necessarily in this function)
initialGameState :: BoardSize -> [Player] -> GameState
initialGameState boardSize players
  = assert (not $ List.null players) $
    InProgress boardSize edges mempty players 0
  where
    edges = Set.fromList $ uncurry mkVector <$>
      ( [ ((x, 0),             LeftEdge)   | x <- [0 .. boardSize - 1] ] <>
        [ ((x, boardSize - 1), RightEdge)  | x <- [0 .. boardSize - 1] ] <>
        [ ((0, y),             TopEdge)    | y <- [0 .. boardSize - 1] ] <>
        [ ((boardSize - 1, y), BottomEdge) | y <- [0 .. boardSize - 1] ]
      )


mkVector :: Point -> Edge -> Vector
mkVector (x, y) TopEdge    = ((x, y),     VectorRight)
mkVector (x, y) BottomEdge = ((x, y + 1), VectorRight)
mkVector (x, y) LeftEdge   = ((x, y),     VectorDown)
mkVector (x, y) RightEdge  = ((x + 1, y), VectorDown)


instance MonadGame Game where
  getOccupant :: Point -> Game (Maybe PlayerIndex)
  getOccupant point = gets (Map.lookup point . view occupants)

  setOccupant :: PlayerIndex -> Point -> Game ()
  setOccupant player point = modify $ occupants %~ (<> Map.singleton point player)

  getEdge :: Point -> Edge -> Game Bool
  getEdge     = undefined

  setEdge :: Point -> Edge -> Game ()  -- you can only switch once from False to True.
  setEdge     = undefined

  getPlayer :: PlayerIndex -> Game Player
  getPlayer     = undefined

  currentPlayer :: Game PlayerIndex
  currentPlayer = undefined

  nextPlayer :: Game ()
  nextPlayer    = undefined
