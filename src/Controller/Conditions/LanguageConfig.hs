-- | Reads/Writes the language in a configuration file.
module Controller.Conditions.LanguageConfig (installHandlers) where

import qualified Control.Exception    as E
import           Control.Exception.Extra
import           Control.Monad
import           System.FilePath
import           System.Directory

import CombinedEnvironment
import Model.Model (Language(..))
import Hails.MVC.Model.ProtectedModel.Reactive

-- | Reads the language config on startup and saves it when modified.
installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let pm = model cenv
  onEvent pm Initialised                $ conditionRead cenv
  onEvent pm LanguageChanged            $ conditionSave cenv

-- | Saves the currently selected language ID String in a configuration file
conditionSave :: CEnv -> IO ()
conditionSave cenv = onViewAsync $ void $ handleAllExceptions (return ()) $ do -- E.handle (anyway (return ())) $ do

  -- Directory and file where results will be saved
  dir <- getAppUserDataDirectory "keera-posture"
  createDirectoryIfMissing True dir
  let file = dir </> "default-language"

  -- Get cur language from the model
  let pm = model cenv
  language <- getter languageField pm

  -- Save
  writeFile file $ languageToLanguageString language

-- | Reads the currently selected language ID String from a configuration file
conditionRead :: CEnv -> IO ()
conditionRead cenv = onViewAsync $ void $ handleAllExceptions (return ()) $ do -- E.handle (anyway (return ())) $ do
  
  -- Directory and file where results will be read
  dir <- getAppUserDataDirectory "keera-posture"
  let file = dir </> "default-language"

  -- Read the language string
  lang <- E.handle (anyway (return "")) $ do
             cs <- fmap lines $ readFile file
             return $ if null cs then "" else head cs
  let language = languageStringToLanguage lang

  -- Update the model
  let pm       = model cenv
  setter languageField pm language

-- | Determines the language based on the language string
languageStringToLanguage :: String -> Maybe Language
languageStringToLanguage "en" = Just English
languageStringToLanguage "es" = Just Spanish
languageStringToLanguage "gl" = Just Galician
languageStringToLanguage _    = Nothing

-- | Determines the language string id based on the language
languageToLanguageString :: Maybe Language -> String
languageToLanguageString (Just English)  = "en"
languageToLanguageString (Just Galician) = "gl"
languageToLanguageString (Just Spanish)  = "es"
languageToLanguageString _               = ""
