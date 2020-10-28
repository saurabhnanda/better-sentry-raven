{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Sentry.Lens where

import Lens.Micro
import Lens.Micro.TH
import Sentry.Types
-- import Sentry.Blank
-- import Data.Maybe

$(makeLensesWith abbreviatedFields ''Scope)
$(makeLensesWith abbreviatedFields ''Event)
$(makeLensesWith abbreviatedFields ''SentryException)
$(makeLensesWith abbreviatedFields ''SentryStacktrace)
$(makeLensesWith abbreviatedFields ''Message)
-- $(makeLensesWith abbreviatedFields ''SentryConfig)
$(makeLensesWith abbreviatedFields ''User)

-- blankSet :: (Blank a) => ASetter s t (Maybe a) (Maybe a) -> (a -> a) -> s -> t
-- blankSet l f = over l (Just . f . (fromMaybe blank))

-- blankGet :: (Blank a) => s -> Getting (Maybe a) s (Maybe a) -> a
-- blankGet x l = fromMaybe blank (x ^. l)

-- blankL getter setter = lens (maybe (Just blank) (Just . getter)) (setter . Just)
