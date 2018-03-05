{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This compat module can be removed once base-4.10 (ghc-8.2) is the minimum
-- required version. Though note that the locking functionality is not in
-- public modules in base-4.10, just in the "GHC.IO.Handle.Lock" module.
module Distribution.Client.Compat.FileLock (
    FileLockingNotSupported(..)
  , LockMode(..)
  , hLock
  , hTryLock
  ) where

#if MIN_VERSION_base(4,10,0)

import GHC.IO.Handle.Lock

#else

-- The remainder of this file is a modified copy
-- of GHC.IO.Handle.Lock from ghc-8.2.x
--
-- The modifications were just to the imports and the CPP, since we do not have
-- access to the HAVE_FLOCK from the ./configure script. We approximate the
-- lack of HAVE_FLOCK with defined(solaris2_HOST_OS) instead since that is the
-- only known major Unix platform lacking flock().

import Control.Exception (Exception)
import Data.Typeable

#if defined(solaris2_HOST_OS)

import Control.Exception (throwIO)
import System.IO (Handle)

#else

import Data.Bits
import Data.Function
import Control.Concurrent.MVar

import Foreign.C.Error
import Foreign.C.Types

import GHC.IO.Handle.Types
import GHC.IO.FD
import GHC.IO.Exception

#if defined(mingw32_HOST_OS)

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
# define PTR_SIZE      4
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
# define PTR_SIZE      8
#else
# error Unknown mingw32 arch

#endif

-- #include <windows.h>

import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Windows

#else /* !defined(mingw32_HOST_OS), so assume unix with flock() */

-- #include <sys/file.h>

#endif /* !defined(mingw32_HOST_OS) */

#endif /* !defined(solaris2_HOST_OS) */


-- | Exception thrown by 'hLock' on non-Windows platforms that don't support
-- 'flock'.
data FileLockingNotSupported = FileLockingNotSupported
  deriving (Typeable, Show)

instance Exception FileLockingNotSupported


-- | Indicates a mode in which a file should be locked.
data LockMode = SharedLock | ExclusiveLock

-- | If a 'Handle' references a file descriptor, attempt to lock contents of the
-- underlying file in appropriate mode. If the file is already locked in
-- incompatible mode, this function blocks until the lock is established. The
-- lock is automatically released upon closing a 'Handle'.
--
-- Things to be aware of:
--
-- 1) This function may block inside a C call. If it does, in order to be able
-- to interrupt it with asynchronous exceptions and/or for other threads to
-- continue working, you MUST use threaded version of the runtime system.
--
-- 2) The implementation uses 'LockFileEx' on Windows and 'flock' otherwise,
-- hence all of their caveats also apply here.
--
-- 3) On non-Windows plaftorms that don't support 'flock' (e.g. Solaris) this
-- function throws 'FileLockingNotImplemented'. We deliberately choose to not
-- provide fcntl based locking instead because of its broken semantics.
--
-- @since 4.10.0.0
hLock :: Handle -> LockMode -> IO ()
hLock h mode = lockImpl h "hLock" mode True >> return ()

-- | Non-blocking version of 'hLock'.
--
-- @since 4.10.0.0
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = lockImpl h "hTryLock" mode False

----------------------------------------

#if defined(solaris2_HOST_OS)

-- | No-op implementation.
lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl _ _ _ _ = throwIO FileLockingNotSupported

#else /* !defined(solaris2_HOST_OS) */

#if defined(mingw32_HOST_OS)

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  wh <- throwErrnoIf (== iNVALID_HANDLE_VALUE) ctx $ c_get_osfhandle fd
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd (fromIntegral sizeof_OVERLAPPED) 0
    let flags = cmode .|. (if block then 0 else lOCKFILE_FAIL_IMMEDIATELY)
    -- We want to lock the whole file without looking up its size to be
    -- consistent with what flock does. According to documentation of LockFileEx
    -- "locking a region that goes beyond the current end-of-file position is
    -- not an error", however e.g. Windows 10 doesn't accept maximum possible
    -- value (a pair of MAXDWORDs) for mysterious reasons. Work around that by
    -- trying 2^32-1.
    fix $ \retry -> c_LockFileEx wh (fromIntegral flags) 0 0xffffffff 0x0 ovrlpd >>= \case
      True  -> return True
      False -> getLastError >>= \err -> if
        | not block && err == (fromIntegral eRROR_LOCK_VIOLATION) -> return False
        | err == (fromIntegral eRROR_OPERATION_ABORTED) -> retry
        | otherwise -> failWith ctx err
  where
    -- See https://msdn.microsoft.com/en-us/library/windows/desktop/bb773368(v=vs.85).aspx
    -- DWORD = 4 bytes
    -- 3 * PTR_SIZE + 2 * DWORD
    -- For 32-bit systems: 3 * 4 + 2 * 4 = 20
    -- For 64-bit systems: 3 * 8 + 2 * 4 = 32
    sizeof_OVERLAPPED = 3 * PTR_SIZE + 2 * 4

    cmode = case mode of
      SharedLock    -> 0
      ExclusiveLock -> lOCKFILE_EXCLUSIVE_LOCK

-- https://msdn.microsoft.com/en-us/library/aa297958.aspx
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365203.aspx
foreign import WINDOWS_CCONV interruptible "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> Ptr () -> IO BOOL

-- NOTE: We hardcode these so that hschs isn't required to compile this.
lOCKFILE_EXCLUSIVE_LOCK, lOCKFILE_FAIL_IMMEDIATELY,
  eRROR_OPERATION_ABORTED, eRROR_LOCK_VIOLATION :: CInt
lOCKFILE_EXCLUSIVE_LOCK   = 0x00000002
lOCKFILE_FAIL_IMMEDIATELY = 0x00000001
eRROR_OPERATION_ABORTED = 0x000003E3
eRROR_LOCK_VIOLATION = 0x00000021

#else /* !defined(mingw32_HOST_OS), so assume unix with flock() */

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  let flags = cmode .|. (if block then 0 else lOCK_NB)
  fix $ \retry -> c_flock fd flags >>= \case
    0 -> return True
    _ -> getErrno >>= \errno -> if
      | not block && errno == eWOULDBLOCK -> return False
      | errno == eINTR -> retry
      | otherwise -> ioException $ errnoToIOError ctx errno (Just h) Nothing
  where
    cmode = case mode of
      SharedLock    -> lOCK_SH
      ExclusiveLock -> lOCK_EX

foreign import ccall interruptible "flock"
  c_flock :: CInt -> CInt -> IO CInt

-- NOTE: We hardcode these so that hschs isn't required to compile this.
lOCK_SH, lOCK_EX, lOCK_NB :: CInt
lOCK_SH = 0x01
lOCK_EX = 0x02
lOCK_NB = 0x04

#endif /* !defined(mingw32_HOST_OS) */

-- | Turn an existing Handle into a file descriptor. This function throws an
-- IOError if the Handle does not reference a file descriptor.
handleToFd :: Handle -> IO FD
handleToFd h = case h of
  FileHandle _ mv -> do
    Handle__{haDevice = dev} <- readMVar mv
    case cast dev of
      Just fd -> return fd
      Nothing -> throwErr "not a file descriptor"
  DuplexHandle{} -> throwErr "not a file handle"
  where
    throwErr msg = ioException $ IOError (Just h)
      InappropriateType "handleToFd" msg Nothing Nothing

#endif /* defined(solaris2_HOST_OS) */

#endif /* MIN_VERSION_base */
