module View.MainWindow.Objects where

-- External imports
import Graphics.UI.Gtk

-- Internal imports
import Paths -- _keera_posture

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadInterface :: IO Builder
loadInterface = do
  builder <- builderNew
  builderPath <- getDataFileName "Interface.glade"
  builderAddFromFile builder builderPath
  return builder
  
onBuilder :: (GObjectClass cls) =>
               (GObject -> cls) -> String -> Builder -> IO cls
onBuilder f s b = builderGetObject b f s

mainMenu :: Builder -> IO Menu
mainMenu bldr = builderGetObject bldr castToMenu "mainMenu"
mainMenuEnableItem :: Builder -> IO MenuItem
mainMenuEnableItem bldr = builderGetObject bldr castToMenuItem "mainMenuEnableItem"
mainMenuCalibrateItem :: Builder -> IO MenuItem
mainMenuCalibrateItem bldr = builderGetObject bldr castToMenuItem "mainMenuCalibrateItem"
-- mainMenuNotificationItem :: Builder -> IO MenuItem
-- mainMenuNotificationItem bldr = builderGetObject bldr castToMenuItem "mainMenuNotificationItem"
-- mainMenuNotificationMenu :: Builder -> IO Menu
-- mainMenuNotificationMenu bldr = builderGetObject bldr castToMenu "mainMenuNotificationMenu"
mainMenuNotifyIconItem :: Builder -> IO CheckMenuItem
mainMenuNotifyIconItem bldr = builderGetObject bldr castToCheckMenuItem "mainMenuNotifyIconItem"
-- mainMenuNotifyBubbleItem :: Builder -> IO CheckMenuItem
-- mainMenuNotifyBubbleItem bldr = builderGetObject bldr castToCheckMenuItem "mainMenuNotifyBubbleItem"
mainMenuNotifyOverlayItem :: Builder -> IO CheckMenuItem
mainMenuNotifyOverlayItem bldr = builderGetObject bldr castToCheckMenuItem "mainMenuNotifyOverlayItem"
mainMenuNotifySoundItem :: Builder -> IO CheckMenuItem
mainMenuNotifySoundItem bldr = builderGetObject bldr castToCheckMenuItem "mainMenuNotifySoundItem"
-- menuitem1 :: Builder -> IO CheckMenuItem
-- menuitem1 bldr = builderGetObject bldr castToCheckMenuItem "menuitem1"
mainMenuPreferences :: Builder -> IO MenuItem
mainMenuPreferences bldr = builderGetObject bldr castToMenuItem "mainMenuPreferences"
mainMenuWebsite :: Builder -> IO MenuItem
mainMenuWebsite bldr = builderGetObject bldr castToMenuItem "mainMenuWebsite"
mainMenuHelp :: Builder -> IO MenuItem
mainMenuHelp bldr = builderGetObject bldr castToMenuItem "mainMenuHelp"
mainMenuClose :: Builder -> IO MenuItem
mainMenuClose bldr = builderGetObject bldr castToMenuItem "mainMenuClose"
mainMenuSeparator0 :: Builder -> IO SeparatorMenuItem
mainMenuSeparator0 bldr = builderGetObject bldr castToSeparatorMenuItem "mainMenuSeparator0"
mainMenuQuit :: Builder -> IO MenuItem
mainMenuQuit bldr = builderGetObject bldr castToMenuItem "mainMenuQuit"
notificationBubble :: Builder -> IO Window
notificationBubble bldr = builderGetObject bldr castToWindow "notificationBubble"
notificationWindow :: Builder -> IO Window
notificationWindow bldr = builderGetObject bldr castToWindow "notificationWindow"
-- notificationEventBox :: Builder -> IO EventBox
-- notificationEventBox bldr = builderGetObject bldr castToEventBox "notificationEventBox"
preferencesDialog :: Builder -> IO Dialog
preferencesDialog bldr = builderGetObject bldr castToDialog "preferencesDialog"
preferencesNotebook :: Builder -> IO Notebook
preferencesNotebook bldr = builderGetObject bldr castToNotebook "preferencesNotebook"
preferencesNotebookMethodsHunchingChkBtn :: Builder -> IO CheckButton
preferencesNotebookMethodsHunchingChkBtn bldr = builderGetObject bldr castToCheckButton "preferencesNotebookMethodsHunchingChkBtn"
preferencesNotebookMethodsSlouchingChkBtn :: Builder -> IO CheckButton
preferencesNotebookMethodsSlouchingChkBtn bldr = builderGetObject bldr castToCheckButton "preferencesNotebookMethodsSlouchingChkBtn"
preferencesNotebookCorrectionSpinBtn :: Builder -> IO SpinButton
preferencesNotebookCorrectionSpinBtn bldr = builderGetObject bldr castToSpinButton "preferencesNotebookCorrectionSpinBtn"
preferencesNotebookDelaySpinBtn :: Builder -> IO SpinButton
preferencesNotebookDelaySpinBtn bldr = builderGetObject bldr castToSpinButton "preferencesNotebookDelaySpinBtn"
preferencesNotebookSourceSpin :: Builder -> IO SpinButton
preferencesNotebookSourceSpin bldr = builderGetObject bldr castToSpinButton "preferencesNotebookSourceSpin"
preferencesNotebookCalibrateBtn :: Builder -> IO Button
preferencesNotebookCalibrateBtn bldr = builderGetObject bldr castToButton "preferencesNotebookCalibrateBtn"
preferencesNotebookNotificationTreeView :: Builder -> IO TreeView
preferencesNotebookNotificationTreeView bldr = builderGetObject bldr castToTreeView "preferencesNotebookNotificationTreeView"
preferencesNotebookLanguageCombo :: Builder -> IO ComboBox
preferencesNotebookLanguageCombo bldr = builderGetObject bldr castToComboBox "preferencesNotebookLanguageCombo"
preferencesNotebookUplinkUpdatesChk :: Builder -> IO CheckButton
preferencesNotebookUplinkUpdatesChk bldr = builderGetObject bldr castToCheckButton "preferencesNotebookUplinkUpdatesChk"
preferencesNotebookUplinkReportsChk :: Builder -> IO CheckButton
preferencesNotebookUplinkReportsChk bldr = builderGetObject bldr castToCheckButton "preferencesNotebookUplinkReportsChk"
preferencesCloseBtn :: Builder -> IO Button
preferencesCloseBtn bldr = builderGetObject bldr castToButton "preferencesCloseBtn"
trayIcon :: Builder -> IO StatusIcon
trayIcon bldr = builderGetObject bldr castToStatusIcon "trayIcon"

preferencesNotebookSoundChkBtn :: Builder -> IO CheckButton
preferencesNotebookSoundChkBtn bldr = builderGetObject bldr castToCheckButton "preferencesNotebookSoundChkBtn"

preferencesNotebookSoundEntry :: Builder -> IO Entry
preferencesNotebookSoundEntry bldr = builderGetObject bldr castToEntry "preferencesNotebookSoundEntry"

preferencesNotebookSoundBtn :: Builder -> IO Button
preferencesNotebookSoundBtn bldr = builderGetObject bldr castToButton "preferencesNotebookSoundBtn"

preferencesNotebookSoundAlign2 :: Builder -> IO Alignment
preferencesNotebookSoundAlign2 bldr = builderGetObject bldr castToAlignment "preferencesNotebookSoundAlign2"

precalibrationWindow :: Builder -> IO Window
precalibrationWindow bldr = builderGetObject bldr castToWindow "precalibrationWindow"

precalibrationImage :: Builder -> IO Image
precalibrationImage bldr = builderGetObject bldr castToImage "precalibrationImage"

precalibrationOkBtn :: Builder -> IO Button
precalibrationOkBtn bldr = builderGetObject bldr castToButton "precalibrationOkBtn"

calibrationWindow :: Builder -> IO Window
calibrationWindow bldr = builderGetObject bldr castToWindow "calibrationWindow"

calibrationImage :: Builder -> IO Image
calibrationImage bldr = builderGetObject bldr castToImage "calibrationImage"

calibrationTitleLbl :: Builder -> IO Label
calibrationTitleLbl bldr = builderGetObject bldr castToLabel "calibrationTitleLbl"

calibrationExplanationLbl :: Builder -> IO Label
calibrationExplanationLbl bldr = builderGetObject bldr castToLabel "calibrationExplanationLbl"

calibrationCloseBtn :: Builder -> IO Button
calibrationCloseBtn bldr = builderGetObject bldr castToButton "calibrationCloseBtn"

calibrationRecalibrateBtn :: Builder -> IO Button
calibrationRecalibrateBtn bldr = builderGetObject bldr castToButton "calibrationRecalibrateBtn"

welcomeWindow :: Builder -> IO Window
welcomeWindow bldr = builderGetObject bldr castToWindow "welcomeWindow"

welcomeImage :: Builder -> IO Image
welcomeImage bldr = builderGetObject bldr castToImage "welcomeImage"

welcomeOkBtn :: Builder -> IO Button
welcomeOkBtn bldr = builderGetObject bldr castToButton "welcomeOkBtn"
