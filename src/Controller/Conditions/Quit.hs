-- | Quit the program when the user requests it
module Controller.Conditions.Quit where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment

-- | Detect when the user activates the menu item 'Quit'
installHandlers :: CEnv -> IO()
installHandlers cenv = do
  menu <- mainMenuQuit $ mainWindowBuilder $ view cenv
  icon <- trayIcon $ mainWindowBuilder $ view cenv

  menuItemActivateField menu =:>
    ( (constW False (statusIconVisibleReactive icon))
      &.& (wrapDo_ mainQuit)
    )

-- -- | Deinstalls the status icon and stops the view, effectively stopping the
-- -- whole program.
-- condition :: CEnv -> IO()
-- condition cenv = do
--   icon <- trayIcon $ mainWindowBuilder $ view cenv
--   statusIconSetVisible icon False
--   mainQuit
