module Controller.Conditions.Config where

import Control.Monad
import Data.Maybe
import Hails.MVC.Controller.Conditions.Config

import CombinedEnvironment
import Hails.MVC.Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  onEvent pm Initialised                       $ defaultRead  myConfigIO app cenv
  onEvent pm CheckUpdatesChanged               $ defaultWrite myConfigIO app cenv
  onEvent pm NotificationEnabledChanged        $ defaultWrite myConfigIO app cenv
  onEvent pm NotificationIconEnabledChanged    $ defaultWrite myConfigIO app cenv
  onEvent pm NotificationSoundEnabledChanged   $ defaultWrite myConfigIO app cenv
  onEvent pm NotificationOverlayEnabledChanged $ defaultWrite myConfigIO app cenv
  onEvent pm NotificationDelayChanged          $ defaultWrite myConfigIO app cenv
  onEvent pm CalibrationParamsChanged          $ defaultWrite myConfigIO app cenv
  onEvent pm CorrectionFactorChanged           $ defaultWrite myConfigIO app cenv
  onEvent pm SoundFilenameChanged              $ defaultWrite myConfigIO app cenv
  where pm  = model cenv
        app = "keera-posture"

myConfigIO :: ConfigIO CEnv
myConfigIO = (myConfigRead, myConfigShow)

myConfigRead :: Maybe String -> CEnv -> IO()
myConfigRead c cenv = do
  when (isJust mConf) $ do
    let (updates, headPos, correct, delay, notif, wIcon, wOver, wSound, wSndFn) = fromJust mConf
    let pm = model cenv
    setter checkUpdatesField               pm updates 
    setter calibrationParamsField          pm headPos 
    setter correctionFactorField           pm correct 
    setter notificationEnabledField        pm notif
    setter notificationDelayField          pm delay   
    setter notificationIconEnabledField    pm wIcon   
    setter notificationOverlayEnabledField pm wOver   
    setter notificationSoundEnabledField   pm wSound  
    setSoundFilename pm wSndFn

  -- Mark whether this is the first time the program is executed
  markFR (isNothing mConf)
  where markFR v = setter firstRunField (model cenv) (Just v)
        mConf    = readConfig =<< c

myConfigShow :: CEnv -> IO String
myConfigShow cenv = do
  let pm = model cenv
  updates <- getter checkUpdatesField               pm
  headPos <- getter calibrationParamsField          pm
  correct <- getter correctionFactorField           pm
  notif   <- getter notificationEnabledField        pm
  delay   <- getter notificationDelayField          pm
  wIcon   <- getter notificationIconEnabledField    pm
  wOver   <- getter notificationOverlayEnabledField pm
  wSound  <- getter notificationSoundEnabledField   pm
  wSndFn  <- getSoundFilename pm
  return $ show (updates, headPos, correct, delay, notif, wIcon, wOver, wSound, wSndFn)
  
type Rect = (Int, Int, Int, Int)

-- Config represents the contents of the config file.
type Config = Config_0_0_0_3

type Config_0_0_0_3 = (Bool, Maybe Rect, Int, Int, Bool, Bool, Bool, Bool, Maybe FilePath)

readConfig :: String -> Maybe Config
readConfig = readConfig_0_0_0_3

readConfig_0_0_0_3 :: String -> Maybe Config
readConfig_0_0_0_3 c = case parsed of
  [(_, _)] -> Just conf
  _        -> Nothing
 where parsed      = reads c
       [(conf, _)] = parsed
