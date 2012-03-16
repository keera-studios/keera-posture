-- | This contains the main controller. Many operations will be
-- implemented in the Controller.* subsystem. This module simply
-- initialises program.
module Controller where

-- External imports
import           Control.Monad
import qualified Control.Exception as E
import           Control.Exception.Extra
import           Data.List
import           Graphics.UI.Gtk.Helpers.MessageDialog
import           Hails.I18N.Gettext
import           Hails.I18N.Language
import           System.Environment
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
startController = handleAllExceptions reportSevereError $ do

  myPutStrLn "Starting Keera Posture"

  installLanguage "keera-posture"
  
  -- Parse command-line args
  args <- parseArgs

  startControllerWithArgs args

startControllerWithArgs :: [ Arg ] -> IO()
startControllerWithArgs [ IncorrectArg ] =
  reportSeverelyError (__ "You invocated Keera Posture with the wrong arguments. Please, read the online documentation or contact support@keera.es.")

startControllerWithArgs ( ShowHelp : _ ) = do
  canStdErr <- hIsOpen stderr
  canStdOut <- hIsOpen stdout
  let can = canStdErr || canStdOut
      hdl = if canStdErr then stderr else stdout
  when can $ do
    hPutStrLn hdl $ __ "USAGE: keera-posture [OPTION]"
    hPutStrLn hdl $ __ "Track your sitting position and warn you when you move"
    hPutStrLn hdl $ ""
    hPutStrLn hdl $ __ "  -h, --help     print this message"
    hPutStrLn hdl $ __ "  -v, --version  print the version info and exit"
    hPutStrLn hdl $ ""
    hPutStrLn hdl $ __ "Report keera-posture bugs to support@keera.es"

startControllerWithArgs ( ShowVersion: _ ) = do
  canStdErr <- hIsOpen stderr
  canStdOut <- hIsOpen stdout
  let can = canStdErr || canStdOut
      hdl = if canStdErr then stderr else stdout
  when can $ do
    hPutStrLn hdl $ "Keera Posture v0.0.1"
    hPutStrLn hdl $ "Copyright (C) 2010-2011 Ivan Perez Dominguez"
    hPutStrLn hdl $ __ "Licence: All rights reserved"
    hPutStrLn hdl $ __ "This software includes modified and unmodified versions of"
    hPutStrLn hdl $ __ "free and open source software. Read the copyright licence,"
    hPutStrLn hdl $ __ "the online documentation or contact support@keera.es to"
    hPutStrLn hdl $ __ "know which libraries Keera Posture depends on and to obtain"
    hPutStrLn hdl $ __ "the modified code of those libraries when that right is"
    hPutStrLn hdl $ __ "granted to you by their licence."
    hPutStrLn hdl   ""
    hPutStrLn hdl $ __ "Written by Ivan Perez Dominguez"

startControllerWithArgs _ = do

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
reportSevereError = reportSeverelyError s
 where s = __ "Keera Posture crashed. Please, contact support@keera.es."

reportSeverelyError :: String -> IO()
reportSeverelyError s = do
  putStrLn s
  E.handle (anyway (putStrLn s)) $ do
    initView
    onViewAsync $ popupError t s >> destroyView
    startView
 where t = __ "Keera Posture"

parseArgs :: IO [ Arg ]
parseArgs = do
  args <- getArgs
  return $ sort $ map toArg args

toArg :: String -> Arg
toArg "-h"        = ShowHelp
toArg "--help"    = ShowHelp
toArg "-v"        = ShowVersion
toArg "--version" = ShowVersion
toArg _           = IncorrectArg

data Arg = IncorrectArg
         | ShowHelp
         | ShowVersion
 deriving (Eq, Ord)
