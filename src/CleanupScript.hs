{-# LANGUAGE RecordWildCards #-}

module CleanupScript where

import Control.Monad
import Data.Char
import Data.Functor
import Data.List

import Actions
import ScriptMonad

liftS :: Action res -> Script res
liftS act = act `Bind` Pure

listDirectory :: [String] -> Script [DirEntry]
listDirectory = liftS . ListDirectory

removeFile, removeDir :: [String] -> Script ()
removeFile = liftS . RemoveFile
removeDir = liftS . RemoveDir

cleanupScript :: [String] -> Script Bool
cleanupScript dir = do
  entries <- listDirectory dir
  case entries of
    []    -> removeDir dir $> True
    items -> do
      let graphicsCount = length $ filter (\item -> anyExt (name item) graphics) items
      let hasKeptFiles = any (\DirEntry { .. } -> typ == File && not (shouldRemove name)) items
      if graphicsCount > 2 || hasKeptFiles
         then pure False
         else do
          childrenRemoved <- forM items $ \DirEntry { .. } -> do
            let fullPath = dir <> [name]
            case typ of
              Dir -> cleanupScript fullPath
              File -> if shouldRemove name
                         then removeFile fullPath $> True
                         else pure False
          let removeThisDir = and childrenRemoved
          when removeThisDir $ removeDir dir
          pure removeThisDir
  where
    shouldRemove name = anyExt name miscFiles
    anyExt name = any (`isSuffixOf` map toLower name)
    miscFiles = [".log", ".cue"] <> graphics
    graphics = [".jpg", ".bmp", ".png"]
