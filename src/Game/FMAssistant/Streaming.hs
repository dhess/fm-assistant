module Game.FMAssistant.Streaming
       ( unpack
       ) where

import Codec.Archive.Tar (Entries, FormatError(..))
import qualified Codec.Archive.Tar as Tar (read, unpack)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as S (readFile, toLazy_)

readEntries :: (Monad m) => ByteString m r -> m (Entries FormatError)
readEntries bs = Tar.read <$> S.toLazy_ bs

unpack :: (MonadIO m) => FilePath -> FilePath -> m ()
unpack destDir tarFile = liftIO $
  runResourceT $
    readEntries (S.readFile tarFile) >>= liftIO . Tar.unpack destDir
