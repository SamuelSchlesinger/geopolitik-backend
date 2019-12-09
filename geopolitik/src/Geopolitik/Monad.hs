module Geopolitik.Monad where

import Servant
import Control.Monad.Except
import Control.Monad.Catch
import Geopolitik.Database

type MonadGeopolitik m = (MonadDatabase m, MonadError ServerError m, MonadCatch m)
