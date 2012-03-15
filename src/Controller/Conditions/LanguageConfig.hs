module Controller.Conditions.LanguageConfig (installHandlers) where

import qualified Control.Exception    as E
import           Control.Exception.Extra
import           Control.Monad
import           System.FilePath
import           System.Directory

import CombinedEnvironment
import Model.Model (Language(..))
import Model.ProtectedModel.Reactive

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let pm = model cenv
  -- pm <- fmap model $ readIORef cenv
  onEvent pm Initialised                $ conditionRead cenv
  onEvent pm LanguageChanged            $ conditionSave cenv

conditionSave :: CEnv -> IO ()
conditionSave cenv = onViewAsync $ void $ handleAllExceptions (return ()) $ do -- E.handle (anyway (return ())) $ do
  dir <- getAppUserDataDirectory "keera-posture"
  createDirectoryIfMissing True dir
  let file = dir </> "default-language"
  -- pm <- fmap model $ readIORef cenv
  let pm = model cenv
  language <- getter languageField pm
  writeFile file $ languageToLanguageString language

conditionRead :: CEnv -> IO ()
conditionRead cenv = onViewAsync $ void $ handleAllExceptions (return ()) $ do -- E.handle (anyway (return ())) $ do
  dir <- getAppUserDataDirectory "keera-posture"
  let file = dir </> "default-language"

  lang <- E.handle (anyway (return "")) $ do
             cs <- fmap lines $ readFile file
             return $ if null cs then "" else head cs
  let language = languageStringToLanguage lang

  -- pm <- fmap model $ readIORef cenv
  let pm = model cenv
  setter languageField pm language

languageStringToLanguage :: String -> Maybe Language
languageStringToLanguage "en" = Just English
languageStringToLanguage "es" = Just Spanish
languageStringToLanguage "gl" = Just Galician
languageStringToLanguage _    = Nothing

languageToLanguageString :: Maybe Language -> String
languageToLanguageString (Just English)  = "en"
languageToLanguageString (Just Galician) = "gl"
languageToLanguageString (Just Spanish)  = "es"
languageToLanguageString _               = ""
