module Controller.Conditions.Status where

import Control.Arrow
import Control.Monad
import Control.Monad.IfElse
import Data.Maybe
import Graphics.UI.Gtk
import Hails.I18N.Gettext
import Hails.MVC.Model.ProtectedModel.UpdatableModel
import Hails.MVC.Model.ProtectedModel.VersionedModel

import CombinedEnvironment
import Model.Model (Status(..))
import Model.ProtectedModel.Reactive
import Paths

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let pm = model cenv

  onEvent pm Initialised   $ condition cenv
  onEvent pm StatusChanged $ condition cenv
  onEvent pm CameraStatusChanged $ condition cenv
  onEvent pm NotificationToggled $ condition cenv
  mapM_ (\ev -> onEvent pm ev (condition cenv)) $ events notificationEnabledField
  mapM_ (\ev -> onEvent pm ev (condition cenv)) $ events notificationIconEnabledField
  onEvent pm updateNotificationEvent $ condition cenv

  -- It tries to embed the icon every two secons until it is embedded
  flip timeoutAdd 2000 $ do
     condition cenv
     fmap not $ statusIconIsEmbedded =<< (trayIcon . mainWindowBuilder . view) cenv

condition :: CEnv -> IO()
condition = onViewAsync . condition'

condition' :: CEnv -> IO ()
condition' cenv = do
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

-- getStatusIcon :: Bool -> Bool -> Status -> Bool -> (StockId, String)
-- getStatusIcon icon -> notif -> status -> updateFound
getStatusIcon :: Bool -> Bool -> Status -> Bool -> Maybe (String, String)
getStatusIcon  _      False  StatusDisabled     _      = Just (statusImages!!1)
-- getStatusIcon  _      False  StatusFinding      _      = Just (statusImages!!8)
-- getStatusIcon  _      _      StatusDisabled     False  = Just (statusImages!!1)
-- Simplification: getStatusIcon _ _ False _ False = Just (statusImages!!1)
getStatusIcon  _      True   StatusFinding      False  = Just (statusImages!!6)
getStatusIcon  _      False  StatusFinding      False  = Just (statusImages!!1)
getStatusIcon  _      True   StatusIdle         False  = Just (statusImages!!0)
getStatusIcon  _      False  StatusIdle         False  = Just (statusImages!!1)
getStatusIcon  True   True   StatusNotifying    False  = Just (statusImages!!2)
getStatusIcon  False  True   StatusNotifying    False  = Just (statusImages!!0)
getStatusIcon  _      False  StatusCallibrating False  = Just (statusImages!!9)
getStatusIcon  _      _      StatusDisabled     True   = Just (statusImages!!4)
-- Simplification: getStatusIcon _ _ False _ True = Just (statusImages!!4)
getStatusIcon  _      True   StatusFinding      True   = Just (statusImages!!7)
getStatusIcon  _      False  StatusFinding      True   = Just (statusImages!!4)
getStatusIcon  _      True   StatusIdle         True   = Just (statusImages!!3)
getStatusIcon  _      False  StatusIdle         True   = Just (statusImages!!4)
getStatusIcon  True   True   StatusNotifying    True   = Just (statusImages!!5)
getStatusIcon  False  True   StatusNotifying    True   = Just (statusImages!!3)
getStatusIcon  _      False  StatusCallibrating True   = Just (statusImages!!10)
getStatusIcon  _      _      _                  _      = Nothing

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
 where update   = sep ++ __ "Update available"
       disabled = sep ++ __ "Disabled"
       finding  = sep ++ __ "Finding"
       calib    = sep ++ __ "Calibrating"
       kpost    = __ "Keera Posture"
       wpost    = __ "Wrong posture"
       sep      = " - "
