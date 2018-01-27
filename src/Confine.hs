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

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Monoid

import Types
import Confine.Internal


-- | Call 'setEdge', then check if 'setOccupant' needs calling on any of the two affected cells and
-- update whatsNext.
move :: MonadGame m => Point -> Edge -> m ()
move = undefined


run' :: GameState -> Game a -> Either GameError a
run' st game = evalState (runExceptT game) st

run :: BoardSize -> [Player] -> Game a -> Either GameError a
run size pls = run' (initialGameState size pls)


------------------------------------------------------------------------------------------------------
-- i'll try to be a bit more pragmatic, less abstract from here on.  it could be worth refactoring
-- this later.

drawBoard :: GameState -> IO ()
drawBoard = undefined

getMove :: GameState -> IO (Point, Edge)
getMove = undefined

play :: IO ()
play = go (initialGameState 10 ["a", "b"])
  where
    go st = do
      drawBoard st
      case getResult st of
        Nothing -> do
          (pt, edg) <- getMove st
          case run' st (move pt edg >> get) of
            Left err -> throwIO . ErrorCall . show $ err
            Right st' -> go st'

        Just r -> putStrLn $ "done: " <> show r
