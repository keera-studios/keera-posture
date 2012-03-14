-- | This module contains a series of conditions that must hold between
-- the view and the model. Most of these conditions can be separated in
-- two conditions: one that must be checked only when the model changes
-- (and updates the view accordingly), and another that must be checked
-- when the view receives an event (and updates the model accordingly).

module Controller.Conditions where

-- Internal libraries
import CombinedEnvironment

-- Internal libraries: specific conditions
import qualified Controller.Conditions.Config                         as Config
import qualified Controller.Conditions.LanguageConfig                 as Language
import qualified Controller.Conditions.Detector                       as Detector
import qualified Controller.Conditions.PreferencesLanguageCombo       as PreferencesLanguage
import qualified Controller.Conditions.MenuEnabled                    as MenuEnabled
import qualified Controller.Conditions.MenuEnabledClick               as MenuEnabledClick
import qualified Controller.Conditions.NotificationMenu               as NotificationMenu
-- import qualified Controller.Conditions.PreferencesDetectionMethods    as PreferencesDetectionMethods
import qualified Controller.Conditions.PreferencesDialog              as PreferencesDialog
import qualified Controller.Conditions.PreferencesDialogDestroy       as PreferencesDialogDestroy
import qualified Controller.Conditions.PreferencesDetectionCorrection as Correction
import qualified Controller.Conditions.PreferencesDetectionDelay      as Delay
import qualified Controller.Conditions.PreferencesDetectionCamera     as Camera
import qualified Controller.Conditions.PreferencesSoundCustomise      as PreferencesSound
import qualified Controller.Conditions.PopupMenu                      as PopupMenu
import qualified Controller.Conditions.Quit                           as Quit
import qualified Controller.Conditions.PreferencesUplink              as PreferencesUplink
import qualified Controller.Conditions.Calibrate                      as Calibrate
import qualified Controller.Conditions.Precalibration                      as Precalibration
import qualified Controller.Conditions.Postcalibration                      as Postcalibration
import qualified Controller.Conditions.NotifySound                    as Sound
import qualified Controller.Conditions.NotifyWindow                   as Window
import qualified Controller.Conditions.UpdateCheck                    as UpdateCheck
import qualified Controller.Conditions.Status                         as Status
import qualified Controller.Conditions.Website                        as Website
import qualified Controller.Conditions.Help                           as Help
import qualified Controller.Conditions.Welcome                        as Welcome

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  Config.installHandlers                      cenv
  Language.installHandlers                    cenv
  Status.installHandlers                      cenv
  Detector.installHandlers                    cenv
  PreferencesLanguage.installHandlers         cenv
  MenuEnabled.installHandlers                 cenv
  MenuEnabledClick.installHandlers            cenv
  NotificationMenu.installHandlers            cenv
  -- PreferencesDetectionMethods.installHandlers cenv
  PreferencesDialog.installHandlers           cenv
  PreferencesDialogDestroy.installHandlers    cenv
  Correction.installHandlers                  cenv
  Delay.installHandlers                       cenv
  Camera.installHandlers                      cenv
  PreferencesSound.installHandlers            cenv
  PreferencesUplink.installHandlers           cenv
  PopupMenu.installHandlers                   cenv
  Quit.installHandlers                        cenv
  Calibrate.installHandlers                   cenv
  Precalibration.installHandlers              cenv
  Postcalibration.installHandlers             cenv
  Sound.installHandlers                       cenv
  Window.installHandlers                      cenv
  UpdateCheck.installHandlers                 cenv
  Website.installHandlers                     cenv
  Help.installHandlers                        cenv
  Welcome.installHandlers                     cenv
