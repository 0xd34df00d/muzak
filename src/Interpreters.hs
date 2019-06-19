{-# LANGUAGE FlexibleContexts, GADTs, ConstraintKinds #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module Interpreters where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.List(intercalate, isSuffixOf)
import Data.Maybe
import Data.String.Interpolate
import Data.Tree
import Data.Tree.Zipper

import Actions
import ScriptMonad

type DirTree = Tree DirEntry

sampleDirTree :: DirTree
sampleDirTree = d "<root>" $ pure $ d "music"
  [ d "Group 1"
    [ d "Album 1"
      [ f "track1.mp3"
      , f "track2.mp3"
      , f "track3.mp3"
      , f "cover.jpg"
      ]
    ]
  , d "Deleted group"
    [ d "Album 1"
      [ f "cover.jpg"
      , f "cue.log"
      ]
    , d "Album 2"
      [ f "cover.jpg"
      , d "scans"
        [ f "front.jpg"
        , f "back.jpg"
        , f "inside.jpg"
        ]
      ]
    , d "Album 3"
      [
      ]
    ]
  ]
  where
    f name' = Node (DirEntry File name') []
    d name' = Node (DirEntry Dir name')

type PureInterpretMonad m = (MonadState DirTree m, MonadError String m, MonadWriter [String] m)

interpretDirTree :: PureInterpretMonad m => Script a -> m a
interpretDirTree (Pure val) = pure val
interpretDirTree (Bind act step) = interpretDirTreeAct act >>= interpretDirTree . step

interpretDirTreeAct :: PureInterpretMonad m => Action a -> m a
interpretDirTreeAct (ListDirectory path) = do
  curTree <- get
  case findPath path curTree of
    Nothing -> throwError [i|Unable to find path #{intercalate "/" path}|]
    Just subs -> pure $ rootLabel <$> subs
interpretDirTreeAct (RemoveFile path) = removeWithChecks path $ \hrPath pos -> do
  when (typ (rootLabel $ tree pos) /= File) $ throwError [i|Not a file at #{hrPath}|]
  let siblings = rootLabel <$> forest pos
  when (shouldKeep `any` siblings) $ throwError [i|Attempted to remove file in an untouchable dir: #{hrPath}|]
  where
    shouldKeep DirEntry { .. } = typ == File
                              && any (`isSuffixOf` map toLower name) [".flac", ".ogg", ".mp3"]

interpretDirTreeAct (RemoveDir path) = removeWithChecks path $ \hrPath pos -> do
  let Node { .. } = tree pos
  when (typ rootLabel /= Dir) $ throwError [i|Not a dir at #{hrPath}|]
  unless (null subForest) $ throwError [i|Dir not empty at #{hrPath}|]

type DirTreePos = TreePos Full DirEntry

removeWithChecks :: PureInterpretMonad m => [String] -> (String -> DirTreePos -> m ()) -> m ()
removeWithChecks path checks = do
  curTree <- get
  let maybePos = findPathPos path $ fromTree curTree
  when (isNothing maybePos) $ throwError [i|Unable to find path #{hrPath}|]
  let Just pos = maybePos
  checks hrPath pos
  case parent $ delete pos of
    Nothing -> throwError [i|Unable to find parent of the deleted item #{hrPath}|]
    Just p -> do
      tell $ pure [i|Removing entry at #{hrPath}|]
      put $ tree $ root p
  where hrPath = intercalate "/" path

findPath :: [String] -> DirTree -> Maybe [DirTree]
findPath path = fmap (subForest . tree) . findPathPos path . fromTree

findPathPos :: [String] -> DirTreePos -> Maybe DirTreePos
findPathPos [] pos = Just pos
findPathPos (pathElem : rest) pos = maybeChildPos (firstChild pos) >>= findPathPos rest
  where maybeChildPos (Just curPos) | name (label curPos) == pathElem = Just curPos
                                    | otherwise = maybeChildPos $ next curPos
        maybeChildPos Nothing = Nothing

runPureScript :: Script a -> DirTree -> (Either String DirTree, [String])
runPureScript script dirTree = runWriter $ runExceptT $ execStateT (interpretDirTree script) dirTree
