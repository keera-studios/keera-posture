-- | Toggles notification status when the user interacts with the status icon
module Controller.Conditions.MenuEnabledClick where

import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive

import CombinedEnvironment
import Model.Model (Status(..), isDisabled, isCalibrating)
import Hails.MVC.Model.ProtectedModel.Reactive

-- Detects interaction with the main window and the tray icon
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  -- View points
  menu <- mainMenuEnableItem $ mainWindowBuilder $ view cenv
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  let anyInteraction = lMerge (menuItemActivateField menu)
                              (statusIconActivateField icon)

  -- Model points
  let stField = mkFieldAccessor statusField (model cenv)

  -- Reactive Rules

  -- Hide menu (whatever happens)
  anyInteraction =:> wrapDo_ (popDownMenu cenv)

  -- Advance state if it was calibrating

  let statusIfMustCalibrate  = anyInteraction `governingR` statusIfNotCalibrating
      -- statusIfMustCalibrate  = liftR2 (\_ s -> s) anyInteraction statusIfNotCalibrating
      statusIfNotCalibrating = stField `guardRO'` (not.isCalibrating)

  statusIfMustCalibrate =:> (nextState `liftW` stField)

  -- let updateStatusRV = modRW (\x _ -> if not (isCalibrating x) then nextState x else x) stField
  -- anyInteraction =:> updateStatusRV

  -- Disable notification if it was calibrating
  let notifEnabled = mkFieldAccessor notificationEnabledField (model cenv)

  statusIfMustCalibrate =:> (isDisabled `liftW` notifEnabled)

nextState :: Status -> Status
nextState StatusDisabled = StatusIdle
nextState _              = StatusDisabled

popDownMenu cenv = onViewAsync $ do
  -- Close the menu if the option is selected
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  menuPopdown menu

-- -- | Closes the menu and toggles notification status
-- condition :: CEnv -> IO()
-- condition cenv = onViewAsync $ do
--   -- Close the menu if the option is selected
--   menu <- mainMenu $ mainWindowBuilder $ view cenv
--   menuPopdown menu
-- 
--   -- status <- getStatus pm
--   -- notifyEnabled <- getter notificationEnabledField pm
-- 
  -- case status of 
  --  StatusDisabled     -> do setStatus pm StatusIdle
  --                           setter notificationEnabledField pm True
  --  StatusCallibrating -> return ()
  --  _                  -> do setStatus pm StatusDisabled
  --                           setter notificationEnabledField pm False

