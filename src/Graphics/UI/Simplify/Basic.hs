module Graphics.UI.Simplify.Basic
  ( Accessor
  , Setter
  , Getter
  )
 where

-- External libraries
import Graphics.UI.Gtk

-- Internal libraries
import Model.ProtectedModel.Reactive

type Accessor a = Builder -> IO a
