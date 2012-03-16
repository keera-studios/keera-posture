-- | Contains basic operations related to the GUI
module View
  (module View
  , module Exported)
  where

-- External libraries
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import View.MainWindow.Objects
import View.MainWindow.InitLanguagesCombo

instance GtkGUI View where
  initialise = createView

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

