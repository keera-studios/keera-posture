module Graphics.UI.Gtk.Reactive
   ( module Graphics.UI.Gtk.Reactive
   , module Exported
   )
  where

-- External libraries
import Control.Monad
import Graphics.UI.Gtk

-- Internal libraries
import CombinedEnvironment
import Graphics.UI.Simplify.Basic
import Graphics.UI.Simplify.Reactive as Exported
import Graphics.UI.Gtk.Helpers.Combo
import View

type ReactiveViewEntry = ReactiveViewField String

reactiveEntry :: Entry -> ReactiveViewEntry
reactiveEntry entry = ReactiveViewField
 { onChange = void . onEditableChanged entry
 , rvfGet   = get entry entryText
 , rvfSet   = \t -> set entry [ entryText := t ]
 }

cenvReactiveEntry :: Accessor Entry -> CEnv -> IO ReactiveViewEntry
cenvReactiveEntry = cenvReactiveField reactiveEntry

cenvReactiveField :: (a -> ReactiveViewField b) -> Accessor a -> CEnv -> IO (ReactiveViewField b)
cenvReactiveField f entryF cenv = 
  fmap f $ entryF $ mainWindowBuilder $ view cenv

cenvReactiveCheckMenuItem :: Accessor CheckMenuItem -> CEnv -> IO (ReactiveViewField Bool)
cenvReactiveCheckMenuItem = cenvReactiveField reactiveCheckMenuItem

reactiveCheckMenuItem :: CheckMenuItem -> ReactiveViewField Bool
reactiveCheckMenuItem item = ReactiveViewField
 { onChange = void . on item checkMenuItemToggled
 , rvfGet   = checkMenuItemGetActive item
 , rvfSet   = checkMenuItemSetActive item
 }

cenvReactiveToggleButton :: ToggleButtonClass a => Accessor a -> CEnv -> IO (ReactiveViewField Bool)
cenvReactiveToggleButton = cenvReactiveField reactiveToggleButton

reactiveToggleButton :: ToggleButtonClass a => a -> ReactiveViewField Bool
reactiveToggleButton item = ReactiveViewField
 { onChange = void . on item toggled
 , rvfGet   = toggleButtonGetActive item
 , rvfSet   = toggleButtonSetActive item
 }

cenvReactiveSpinButton :: SpinButtonClass a => Accessor a -> CEnv -> IO (ReactiveViewField Int)
cenvReactiveSpinButton = cenvReactiveField reactiveSpinButton

reactiveSpinButton :: SpinButtonClass a => a -> ReactiveViewField Int
reactiveSpinButton item = ReactiveViewField
 { onChange = void . onValueSpinned item
 , rvfGet   = spinButtonGetValueAsInt item
 , rvfSet   = spinButtonSetValue item . fromIntegral
 }

cenvReactiveTypedComboBoxUnsafe :: (Eq a) => ListStore a -> Accessor ComboBox -> CEnv -> IO (ReactiveViewField a)
cenvReactiveTypedComboBoxUnsafe ls = cenvReactiveField (reactiveTypedComboBoxUnsafe ls)

reactiveTypedComboBoxUnsafe :: (Eq a) => ListStore a -> ComboBox -> ReactiveViewField a
reactiveTypedComboBoxUnsafe ls item = ReactiveViewField
 { onChange = void . on item changed
 , rvfGet   = typedComboBoxGetSelectedUnsafe (item, ls)
 , rvfSet   = typedComboBoxSetSelected (item, ls)
 }
