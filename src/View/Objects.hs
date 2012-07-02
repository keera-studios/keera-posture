{-# LANGUAGE TemplateHaskell #-}
module View.Objects where

-- External imports
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Builder
import Hails.Graphics.UI.Gtk.THBuilderAccessor

-- Internal imports
import Paths

loadInterface :: IO Builder
loadInterface = loadDefaultInterface getDataFileName

gtkBuilderAccessor "mainMenu"                                   "Menu"
gtkBuilderAccessor "mainMenuEnableItem"                         "MenuItem"
gtkBuilderAccessor "mainMenuCalibrateItem"                      "MenuItem"
gtkBuilderAccessor "mainMenuNotifyIconItem"                     "CheckMenuItem"
gtkBuilderAccessor "mainMenuNotifyOverlayItem"                  "CheckMenuItem"
gtkBuilderAccessor "mainMenuNotifySoundItem"                    "CheckMenuItem"
gtkBuilderAccessor "mainMenuPreferences"                        "MenuItem"
gtkBuilderAccessor "mainMenuWebsite"                            "MenuItem"
gtkBuilderAccessor "mainMenuHelp"                               "MenuItem"
gtkBuilderAccessor "mainMenuClose"                              "MenuItem"
gtkBuilderAccessor "mainMenuSeparator0"                         "SeparatorMenuItem"
gtkBuilderAccessor "mainMenuQuit"                               "MenuItem"
gtkBuilderAccessor "notificationBubble"                         "Window"
gtkBuilderAccessor "notificationWindow"                         "Window"
gtkBuilderAccessor "notificationEventBox"                       "EventBox"
gtkBuilderAccessor "preferencesDialog"                          "Dialog"
gtkBuilderAccessor "preferencesNotebook"                        "Notebook"
gtkBuilderAccessor "preferencesNotebookMethodsHunchingChkBtn"   "CheckButton"
gtkBuilderAccessor "preferencesNotebookMethodsSlouchingChkBtn"  "CheckButton"
gtkBuilderAccessor "preferencesNotebookCorrectionSpinBtn"       "SpinButton"
gtkBuilderAccessor "preferencesNotebookDelaySpinBtn"            "SpinButton"
gtkBuilderAccessor "preferencesNotebookSourceSpin"              "SpinButton"
gtkBuilderAccessor "preferencesNotebookCalibrateBtn"            "Button"
gtkBuilderAccessor "preferencesNotebookNotificationTreeView"    "TreeView"
gtkBuilderAccessor "preferencesNotebookLanguageCombo"           "ComboBox"
gtkBuilderAccessor "preferencesNotebookUplinkUpdatesChk"        "CheckButton"
gtkBuilderAccessor "preferencesNotebookUplinkReportsChk"        "CheckButton"
gtkBuilderAccessor "preferencesCloseBtn"                        "Button"
gtkBuilderAccessor "trayIcon"                                   "StatusIcon"
gtkBuilderAccessor "preferencesNotebookSoundChkBtn"             "CheckButton"
gtkBuilderAccessor "preferencesNotebookSoundEntry"              "Entry"
gtkBuilderAccessor "preferencesNotebookSoundBtn"                "Button"
gtkBuilderAccessor "preferencesNotebookSoundAlign2"             "Alignment"
gtkBuilderAccessor "precalibrationWindow"                       "Window"
gtkBuilderAccessor "precalibrationImage"                        "Image"
gtkBuilderAccessor "precalibrationOkBtn"                        "Button"
gtkBuilderAccessor "calibrationWindow"                          "Window"
gtkBuilderAccessor "calibrationImage"                           "Image"
gtkBuilderAccessor "calibrationTitleLbl"                        "Label"
gtkBuilderAccessor "calibrationExplanationLbl"                  "Label"
gtkBuilderAccessor "calibrationCloseBtn"                        "Button"
gtkBuilderAccessor "calibrationRecalibrateBtn"                  "Button"
gtkBuilderAccessor "welcomeWindow"                              "Window"
gtkBuilderAccessor "welcomeImage"                               "Image"
gtkBuilderAccessor "welcomeOkBtn"                               "Button"
