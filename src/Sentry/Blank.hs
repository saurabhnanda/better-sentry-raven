{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Sentry.Blank where

import GHC.Generics
import Data.Aeson as Aeson

class Blank a where
  blank :: a

  default blank :: (Generic a, GBlank (Rep a)) => a
  blank = to gBlank

class GBlank f where
  gBlank :: f p

instance (GBlank f, GBlank g) => GBlank (f :*: g) where
  gBlank = gBlank :*: gBlank

instance (GBlank c) => GBlank (D1 x c) where
  gBlank = M1 gBlank

instance (GBlank s) => GBlank (C1 x s) where
  gBlank = M1 gBlank

instance (Blank t) => GBlank (S1 m (Rec0 t)) where
  gBlank = M1 (K1 blank)

instance Blank (Maybe a) where
  blank = Nothing

instance Blank () where
  blank = ()

instance Blank Aeson.Value where
  blank = Aeson.Null

instance Blank [a] where
  blank = []

instance Blank (a -> a) where
  blank = Prelude.id

instance Blank (a -> b -> a) where
  blank = Prelude.const

