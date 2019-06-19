{-# LANGUAGE GADTs #-}

module Actions where

data EntryType = Dir | File deriving (Eq, Ord, Show)

data DirEntry = DirEntry
  { typ :: EntryType
  , name :: FilePath
  } deriving (Eq, Ord, Show)

data Action res where
  ListDirectory :: FilePath -> Action [DirEntry]
  RemoveFile    :: FilePath -> Action ()
  RemoveDir     :: FilePath -> Action ()
