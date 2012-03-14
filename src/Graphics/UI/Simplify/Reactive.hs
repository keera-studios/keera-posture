module Graphics.UI.Simplify.Reactive where

-- External libraries
import Control.Monad
import Hails.MVC.Controller.ConditionDirection

-- Internal libraries
import CombinedEnvironment
import Model.ProtectedModel
import Model.ProtectedModel.Reactive

data ReactiveViewField b = ReactiveViewField
 { onChange :: IO () -> IO ()
 , rvfGet   :: IO b
 , rvfSet   :: b -> IO ()
 }

type Condition = CEnv -> IO()

(=:=) :: (Eq b, ReactiveReadWriteField a b) => ReactiveViewField b -> a -> Condition
(=:=) vField mField cenv = do
  let pm = model cenv
  onChange vField condVM
  mapM_ (\ev -> onEvent pm ev condMV) evs
 where evs    = Initialised : events mField
       condMV = cond vField mField MV cenv
       condVM = cond vField mField VM cenv

cond :: (Eq b, ReactiveReadWriteField a b) => ReactiveViewField b -> a -> ConditionDirection -> Condition
cond vField mField cd cenv = onViewAsync $ do
  let pm = model cenv
  mShould <- rvfGet vField
  vShould <- getter mField pm
  let diff = mShould /= vShould
  
  case cd of
   MV -> when diff $ rvfSet vField vShould
   VM -> when diff $ setter mField pm mShould
