-- | Contains basic operations related to the GUI
module View
  ( module View
  , module Exported
  )
  where

-- External libraries
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import View.Objects
import View.InitLanguagesCombo

-- | Initialises the GUI. This must be called before
-- any other GUI operation.
initView :: IO ()
initView = void initGUI

-- | Starts a thread for the view.
startView :: IO ()
startView = mainGUI

-- | Executes an operation on the view thread synchronously
onViewSync :: IO a -> IO a
onViewSync = postGUISync

-- | Executes an operation on the view thread asynchronously
onViewAsync :: IO () -> IO ()
onViewAsync = postGUIAsync

-- | Destroys the view thread
destroyView :: IO ()
destroyView = mainQuit


instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = mainWindowBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { mainWindowBuilder :: Builder
  , languageListStore :: LanguageListStore
  -- , camerasListStore  :: CameraListStore
  -- , statusIcon :: StatusIcon
  }

createView :: IO View
createView = do
  bldr  <- loadInterface

  langs <- initLanguagesCombo bldr
  -- cams  <- initCamerasCombo bldr
  -- icon <- trayIcon bldr

  return
    View
      { mainWindowBuilder = bldr
      , languageListStore = langs
      -- , camerasListStore  = cams
      -- , statusIcon        = icon
      }

