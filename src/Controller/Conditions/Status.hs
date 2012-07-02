module Controller.Conditions.Status where

import Control.Arrow
import Control.Monad
import Control.Monad.IfElse
import Data.Maybe
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.UpdatableModel
import Hails.MVC.Model.ProtectedModel.VersionedModel

import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive
import Paths
import I18N.Strings

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  onEvents pm evs $ condition cenv

  -- It tries to embed the icon every two secons until it is embedded
  flip timeoutAdd 2000 $ do
     condition cenv
     fmap not $ statusIconIsEmbedded =<< (trayIcon . mainWindowBuilder . view) cenv

  where pm  = model cenv
        evs = [ Initialised, StatusChanged, CameraStatusChanged 
              , NotificationEnabledChanged, updateNotificationEvent
              ]
              ++ events notificationEnabledField
              ++ events notificationIconEnabledField

condition :: CEnv -> IO()
condition cenv = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv
  icon          <- trayIcon $ mainWindowBuilder vw 
  status        <- getStatus pm
  notifyEnabled <- getter notificationEnabledField pm
  iconEnabled   <- getter notificationIconEnabledField pm

  newV <- getMaxVersionAvail pm
  curV <- getVersion pm

  let updateFound = isJust newV && curV < fromJust newV
      stView      = getStatusIcon iconEnabled notifyEnabled status updateFound

  awhen stView $ \(imgFn, tooltip) -> do
    getDataFileName imgFn >>= statusIconSetFromFile icon
    statusIconSetTooltip icon tooltip
    statusIconSetVisible icon True

getStatusIcon :: Bool -> Bool -> Status -> Bool -> Maybe (String, String)
getStatusIcon  _      _      StatusDisabled     False  = Just (statusImages!!1)
getStatusIcon  _      True   StatusFinding      False  = Just (statusImages!!6)
getStatusIcon  _      False  StatusFinding      False  = Just (statusImages!!1)
getStatusIcon  _      True   StatusIdle         False  = Just (statusImages!!0)
getStatusIcon  _      False  StatusIdle         False  = Just (statusImages!!1)
getStatusIcon  True   True   StatusNotifying    False  = Just (statusImages!!2)
getStatusIcon  False  True   StatusNotifying    False  = Just (statusImages!!0)
getStatusIcon  _      False  StatusCallibrating False  = Just (statusImages!!9)
getStatusIcon  _      _      StatusDisabled     True   = Just (statusImages!!4)
getStatusIcon  _      True   StatusFinding      True   = Just (statusImages!!7)
getStatusIcon  _      False  StatusFinding      True   = Just (statusImages!!4)
getStatusIcon  _      True   StatusIdle         True   = Just (statusImages!!3)
getStatusIcon  _      False  StatusIdle         True   = Just (statusImages!!4)
getStatusIcon  True   True   StatusNotifying    True   = Just (statusImages!!5)
getStatusIcon  False  True   StatusNotifying    True   = Just (statusImages!!3)
getStatusIcon  _      False  StatusCallibrating True   = Just (statusImages!!10)
getStatusIcon  _      _      _                  _      = Nothing

  -- Patterns not matched: _    True  StatusCallibrating _
  --                       True True  StatusDisabled     False
  --                       True False StatusNotifying    _

statusImages :: [(String, String)]
statusImages =
  [ ("icon-good-posture.png",           kpost)
  , ("icon-disabled.png",               kpost ++ disabled)
  , ("icon-bad-posture.png",            wpost)
  , ("icon-good-posture-update.png",    kpost ++ update)
  , ("icon-disabled-update.png",        kpost ++ disabled ++ update)
  , ("icon-bad-posture-update.png",     wpost ++ update)
  , ("icon-unknown-posture.png",        kpost ++ finding)
  , ("icon-unknown-posture-update.png", kpost ++ finding ++ update)
  , ("icon-calibrating.png",            kpost ++ calib)
  , ("icon-calibrating-update.png",     kpost ++ calib ++ update)
  ]
 where update   = sep ++ strUpdateAvailable
       disabled = sep ++ strDisabled
       finding  = sep ++ strFinding
       calib    = sep ++ strCalibrating
       kpost    = "Keera Posture"
       wpost    = strWrongPosture
       sep      = " - "
