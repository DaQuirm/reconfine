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

import Control.Monad.State
import Control.Monad.Except
import Data.Set as Set
import Data.Map as Map

import Types


type Game = ExceptT GameError (State (GameState Priv))

type Priv = (Set Vector, Map Point Player)

type Vector = (Point, VectorDir)  -- (slight abuse of linear algebar terminology)

data VectorDir = VectorDown | VectorRight
  deriving (Eq, Ord, Show)


mkVector :: Point -> Edge -> Vector
mkVector (x, y) TopEdge    = ((x, y),     VectorRight)
mkVector (x, y) BottomEdge = ((x, y + 1), VectorRight)
mkVector (x, y) LeftEdge   = ((x, y),     VectorDown)
mkVector (x, y) RightEdge  = ((x + 1, y), VectorDown)
