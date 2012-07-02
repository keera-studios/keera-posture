-- | Shows the popup menu when the user right-clicks the icon
module I18N.Strings where

import Hails.I18N.Gettext

explanation1 :: String
explanation1 = __ "This is the calibration assistant.\n" ++
               __ "Keera Posture will look at you now."

title1 :: String
title1 = __ "<b>Calibration</b>"

strUpdateAvailable :: String
strUpdateAvailable = __ "Update available"

strDisabled :: String
strDisabled = __ "Disabled"

strFinding :: String
strFinding = __ "Finding"

strCalibrating :: String
strCalibrating = __ "Calibrating"

strWrongPosture :: String
strWrongPosture = __ "Wrong posture"

strOpenSoundFile :: String
strOpenSoundFile = __ "Open sound file"

strSoundFiles :: String
strSoundFiles = __ "WAV Sound files"

strDisable :: String
strDisable = __ "Disable"

strEnable :: String
strEnable = __ "Enable"
