{-|
Module      : Game.FMAssistant.Version
Description : A game version monad
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Game.FMAssistant.Version
       ( -- * MonadVersion class
         MonadVersion(..)
       , defaultUserDirPath
       , FM16T(..)
       , FM16
       , runFM16
       ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont, ContT)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)
import Control.Monad.Writer (MonadWriter)
import Path ((</>), mkRelDir)

import Game.FMAssistant.Types (VersionDirPath(..), UserDirPath(..))
import Game.FMAssistant.Util (defaultSteamDir)

-- | A monad which provides a context for a particular version of the
-- game.
class (Monad m) => MonadVersion m where
  -- | The game version.
  version :: m VersionDirPath

-- | The default user directory, where save games and most mod types
-- live.
defaultUserDirPath :: (MonadThrow m, MonadIO m, MonadVersion m) => m UserDirPath
defaultUserDirPath =
  do steamDir <- defaultSteamDir
     versionDir <- _versionDirPath <$> version
     return $ UserDirPath (steamDir </> versionDir)

instance (MonadVersion m) => MonadVersion (IdentityT m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (ContT r m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (ExceptT e m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (ListT m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (MaybeT m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (ReaderT r m) where
  version = lift version

instance (MonadVersion m, Monoid w) => MonadVersion (LazyRWS.RWST r w s m) where
  version = lift version

instance (MonadVersion m, Monoid w) => MonadVersion (StrictRWS.RWST r w s m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (LazyState.StateT s m) where
  version = lift version

instance (MonadVersion m) => MonadVersion (StrictState.StateT s m) where
  version = lift version

instance (MonadVersion m, Monoid w) => MonadVersion (LazyWriter.WriterT w m) where
  version = lift version

instance (MonadVersion m, Monoid w) => MonadVersion (StrictWriter.WriterT w m) where
  version = lift version

-- | The FM16 game version monad.
newtype FM16T m a =
  FM16T {runFM16T :: m a}
  deriving (Functor,Alternative,Applicative,Monad,MonadFix,MonadPlus,MonadThrow,MonadCatch,MonadMask,MonadCont,MonadIO,MonadReader r,MonadError e,MonadWriter w,MonadState s,MonadRWS r w s)

instance MonadTrans FM16T where
  lift = FM16T


instance MonadIO m => MonadVersion (FM16T m) where
  -- |
  -- >>> runFM16 version
  -- Football Manager 2016
  version = return $ VersionDirPath $ $(mkRelDir "Football Manager 2016")

-- | An 'FM16T' transformer specialized to 'IO'.
type FM16 = FM16T IO

-- | Run an 'IO' action in the 'FM16' monad.
runFM16 :: FM16 a -> IO a
runFM16 = runFM16T
