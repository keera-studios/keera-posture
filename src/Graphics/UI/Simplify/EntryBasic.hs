module Graphics.UI.Simplify.EntryBasic
    (installHandlers)
  where

-- External libraries
import Control.Arrow
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk

-- Internal libraries
import CombinedEnvironment
import Controller.ConditionDirection
import View
import Model.ProtectedModel

type Accessor a = Builder -> IO a
type Setter a = ProtectedModel -> a -> IO()
type Getter a = ProtectedModel -> IO a

installHandlers :: [ ModelEvent ] -> Accessor Entry -> Setter String -> Getter String -> CRef -> IO()
installHandlers evs entryF setter getter cref = do
  (vw, pm) <- fmap (view &&& model) $ readIORef cref
  entry  <- entryF $ mainWindowBuilder vw
  entry `onEditableChanged` condition cref VM entryF setter getter
  mapM_ (\ev -> onEvent pm ev (condition cref MV entryF setter getter)) evs

-- | Enforces the condition
condition :: CRef -> ConditionDirection -> Accessor Entry -> Setter String -> Getter String ->IO()
condition cref cd entryF setter getter = onViewAsync $ do
  (vw, pm) <- fmap (view &&& model) $ readIORef cref
  entry            <- entryF $ mainWindowBuilder vw
  t                <- get entry entryText
  curModelValue    <- getter pm
  -- let viewValueShould = fromMaybe "" curModelValue

  case cd of
   MV -> when (curModelValue /= t) $ set entry [ entryText := curModelValue ]
   VM -> when (curModelValue /= t) $ setter pm t
