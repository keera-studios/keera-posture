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

title2 :: String
title2 = __ "<b>Calibration failed</b>"

title3 :: String
title3 = __ "<b>Calibration succeeded</b>"

explanation2 :: String
explanation2 = __ "Keera Posture was unable to detect you. Make sure that the camera is\n" ++
               __ "plugged and that you are sitting in front of the camera, neither too\n" ++ 
               __ "close nor too far."

explanation3 :: String
explanation3 = __ "Keera Posture has detected you successfully!\n" ++
               __ "You can close the assistant to proceed or\nrecalibrate if you were not sitting correctly."

strLangAuto :: String
strLangAuto = __ "Automatic selection"
