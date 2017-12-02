{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
#ifdef ETA_VERSION
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
#endif

module Distribution.Compat.CreatePipe (createPipe) where

import System.IO (Handle, hSetEncoding, localeEncoding)

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.Stack

-- The mingw32_HOST_OS CPP macro is GHC-specific
#ifdef mingw32_HOST_OS
import qualified Prelude
import Control.Exception (onException)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff)
import GHC.IO.FD (mkFD)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Handle.FD (mkHandleFromFD)
import System.IO (IOMode(ReadMode, WriteMode))
#elif defined ghcjs_HOST_OS
#elif defined(ETA_VERSION)
import System.Posix.Types (Channel)
import GHC.IO.Handle.FD (fdToHandle)
import Java
#else
import System.Posix.IO (fdToHandle)
import qualified System.Posix.IO as Posix
#endif

createPipe :: IO (Handle, Handle)
-- The mingw32_HOST_OS CPP macro is GHC-specific
#ifdef mingw32_HOST_OS
createPipe = do
    (readfd, writefd) <- allocaArray 2 $ \ pfds -> do
        throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 ({- _O_BINARY -} 32768)
        readfd <- peek pfds
        writefd <- peekElemOff pfds 1
        return (readfd, writefd)
    (do readh <- fdToHandle readfd ReadMode
        writeh <- fdToHandle writefd WriteMode
        hSetEncoding readh localeEncoding
        hSetEncoding writeh localeEncoding
        return (readh, writeh)) `onException` (close readfd >> close writefd)
  where
    fdToHandle :: CInt -> IOMode -> NoCallStackIO Handle
    fdToHandle fd mode = do
        (fd', deviceType) <- mkFD fd mode (Just (Stream, 0, 0)) False False
        mkHandleFromFD fd' deviceType "" mode False Nothing

    close :: CInt -> IO ()
    close = throwErrnoIfMinus1_ "_close" . c__close
      where _ = callStack -- TODO: attach call stack to exception

    _ = callStack -- TODO: attach call stack to exceptions

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> Prelude.IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> Prelude.IO CInt
#elif defined ghcjs_HOST_OS
createPipe = error "createPipe"
  where
    _ = callStack
#elif defined(ETA_VERSION)
createPipe = do
    p <- createPipe'
    let readfd  = superCast (pipeSource p)
        writefd = superCast (pipeSink   p)
    readh  <- fdToHandle readfd
    writeh <- fdToHandle writefd
    hSetEncoding readh localeEncoding
    hSetEncoding writeh localeEncoding
    return (readh, writeh)

data {-# CLASS "java.nio.channels.Pipe" #-} Pipe = Pipe (Object# Pipe)

data {-# CLASS "java.nio.channels.Pipe$SourceChannel" #-} SourceChannel = SourceChannel (Object# SourceChannel)
  deriving Class

type instance Inherits SourceChannel = '[Channel]

data {-# CLASS "java.nio.channels.Pipe$SinkChannel" #-} SinkChannel = SinkChannel (Object# SinkChannel)
  deriving Class

type instance Inherits SinkChannel = '[Channel]

foreign import java unsafe "@static java.nio.channels.Pipe.open" createPipe'
  :: IO Pipe

foreign import java unsafe "source" pipeSource
  :: Pipe -> SourceChannel

foreign import java unsafe "sink" pipeSink
  :: Pipe -> SinkChannel
#else
createPipe = do
    (readfd, writefd) <- Posix.createPipe
    readh <- fdToHandle readfd
    writeh <- fdToHandle writefd
    hSetEncoding readh localeEncoding
    hSetEncoding writeh localeEncoding
    return (readh, writeh)
  where
    _ = callStack
#endif
