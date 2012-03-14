{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Model.ProtectedModel.Reactive where

import Model.ProtectedModel.ProtectedModelInternals
import Model.ReactiveModel.ModelEvents

type Setter a = ProtectedModel -> a -> IO()
type Getter a = ProtectedModel -> IO a

class ReactiveField a b | a -> b where
  events :: a -> [ModelEvent]

class ReactiveField a b => ReactiveReadField a b where
  getter :: a -> Getter b

class ReactiveWriteField a b where
  setter :: a -> Setter b

class (ReactiveField a b, ReactiveReadField a b, ReactiveWriteField a b) => ReactiveReadWriteField a b where

data ReactiveElement a = ReactiveElement
  { reEvents :: [ ModelEvent ]
  , reSetter :: Setter a
  , reGetter :: Getter a
  }

instance ReactiveField (ReactiveElement a) a where
 events = reEvents

instance ReactiveReadField (ReactiveElement a) a where
 getter = reGetter

instance ReactiveWriteField (ReactiveElement a) a where
 setter = reSetter

instance ReactiveReadWriteField (ReactiveElement a) a where
