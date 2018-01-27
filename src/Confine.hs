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
import Control.Monad.Except

import Types
import Confine.Internal


getOccupant :: MonadGame priv m => Point -> m (Maybe Player)
getOccupant = undefined

setOccupant :: MonadGame priv m => Player -> Point -> m ()
setOccupant = undefined


getEdge :: MonadGame priv m => Point -> Edge -> m Bool
getEdge = undefined

-- | you can only switch once from False to True.
setEdge :: MonadGame priv m => Point -> Edge -> m ()
setEdge = undefined


-- | 'setEdge', then potentially 'setOccupant' of (one of) the two affected cells.
move :: MonadGame priv m => Player -> Point -> Edge -> m (GameState priv)
move = undefined


run :: Int -> Game a -> Either GameError a
run = undefined
