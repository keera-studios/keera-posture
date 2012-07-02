module View.InitLanguagesCombo where

-- External libraries
import Graphics.UI.Gtk hiding (Language)
import Hails.Graphics.UI.Gtk.Helpers.Combo

-- Internal libraries
import I18N.Strings
import Model.Model (Language(..))
import View.Objects

type LanguageListStore = ListStore (Maybe Language)

initLanguagesCombo :: Builder -> IO LanguageListStore
initLanguagesCombo bldr = do
  st <- listStoreNew $ map fst supportedLanguages

  cb <- preferencesNotebookLanguageCombo bldr
  addTextColumn cb st (`lookup` supportedLanguages)

  set cb [ comboBoxActive := 0 ]
  return st

supportedLanguages :: [ (Maybe Language, String) ]
supportedLanguages = [ (Nothing,       strLangAuto)
                     , (Just English,  "English")
                     , (Just Galician, "Galego")
                     , (Just Spanish,  "Español")
                     ]
