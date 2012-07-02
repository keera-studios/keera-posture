{-# LANGUAGE DeriveDataTypeable #-}
-- | This contains the main controller. Many operations will be
-- implemented in the Controller.* subsystem. This module simply
-- initialises program.
module Controller where

-- External imports
import           Control.Monad
import qualified Control.Exception as E
import           Control.Exception.Extra
import           Hails.Graphics.UI.Gtk.Helpers.MessageDialog
import           Hails.I18N.Gettext
import           Hails.I18N.Language
import           System.Console.CmdArgs
import           System.IO

-- Internal imports
import CombinedEnvironment
import Controller.Conditions
import Model.Model
import MyIO

-- | Starts the program by creating the model,
-- the view, starting all the concurrent threads,
-- installing the hanlders for all the conditions
-- and starting the view.
startController :: IO ()
startController = do 
   handleAllExceptions reportSevereError $ do
     myPutStrLn "Starting Keera Posture"
     installLanguage "keera-posture"
   ags <- cmdArgs defArgs
   handleAllExceptions reportSevereError $ startControllerWithArgs ags

startControllerWithArgs :: Args -> IO()
startControllerWithArgs ags
 | licence ags
 = do canStdErr <- hIsOpen stderr
      canStdOut <- hIsOpen stdout
      let can = canStdErr || canStdOut
          hdl = if canStdErr then stderr else stdout
      when can $ mapM_ (hPutStrLn hdl)
        [ "Keera Posture v0.0.2"
        , "Copyright (C) 2010-2012 Ivan Perez Dominguez"
        , __ "Licence: All rights reserved"
        , __ "This software includes modified and unmodified versions of"
        , __ "free and open source software. Read the copyright licence,"
        , __ "the online documentation or contact support@keera.es to"
        , __ "know which libraries Keera Posture depends on and to obtain"
        , __ "the modified code of those libraries when that right is"
        , __ "granted to you by their licence."
        , ""
        , __ "Written by Ivan Perez Dominguez"
        ]
 | otherwise
 = startKeeraMain

startKeeraMain :: IO ()
startKeeraMain = do

  -- Initialise the visual layer
  initView
  
  -- Create an empty model
  cenv <- createCEnv emptyBM
  
  -- Install the model and view handlers
  installHandlers cenv
  
  -- Modify the system initialisation
  initialiseSystem $ model cenv
  
  -- Run the view
  startView

reportSevereError :: IO ()
reportSevereError = reportSeverelyError $
 __ "Keera Posture crashed. Please, contact support@keera.es."

reportSeverelyError :: String -> IO()
reportSeverelyError s = do
  putStrLn s
  E.handle (anyway (putStrLn s)) $ do
    initView
    onViewAsync $ popupError t s >> destroyView
    startView
 where t = __ "Keera Posture"

data Args = Args
   { licence :: Bool }
 deriving (Show, Data, Typeable)

-- This is the cmdArgs-based CLI interface definition
defArgs :: Args
defArgs = Args
          { licence = def
          &= help "Shows the licence and exits"
          }
         &= program "keera-posture"
         &= summary "Keera Posture 0.0.2 (c) 2010-2012 Ivan Perez - Keera Studios"
         &= details [ __ "Report bugs to ivan.perez@keera.es"
                    , __ "Find more about Keera Posture at http://keera.es"
                    , __ "and http://github.com/ivanperez-keera/keera-posture"
                    ]
