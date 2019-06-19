{-# LANGUAGE GADTs #-}

module ScriptMonad where

import Control.Monad

import Actions

data Script res where
  Pure :: res -> Script res
  Bind :: Action r1 -> (r1 -> Script r2) -> Script r2

instance Functor Script where
  fmap f (Pure res) = Pure $ f res
  fmap f (Bind act step) = Bind act $ fmap f . step

instance Applicative Script where
  pure = Pure
  Pure f        <*> script = f <$> script
  Bind act step <*> script = Bind act $ \r -> step r <*> script

instance Monad Script where
  Pure val      >>= f = f val
  Bind act step >>= f = Bind act $ step >=> f
