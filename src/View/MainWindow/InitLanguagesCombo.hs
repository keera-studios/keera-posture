module View.MainWindow.InitLanguagesCombo where

-- External libraries
import Graphics.UI.Gtk hiding (Language)

-- Internal libraries
import Graphics.UI.Gtk.Helpers.Combo
import Gettext
import View.MainWindow.Objects
import Model.Model (Language(..))

type LanguageListStore = ListStore (Maybe Language)

initLanguagesCombo :: Builder -> IO LanguageListStore
initLanguagesCombo bldr = do
  st <- listStoreNew $ map fst supportedLanguages

  cb <- preferencesNotebookLanguageCombo bldr
  addTextColumn cb st (`lookup` supportedLanguages)

  set cb [ comboBoxActive := 0 ]
  return st

supportedLanguages :: [ (Maybe Language, String) ]
supportedLanguages = [ (Nothing,       __ "Automatic selection")
                     , (Just English,  "English")
                     , (Just Galician, "Galego")
                     , (Just Spanish,  "Español")
                     ]
