{-# LANGUAGE CPP #-}
module Distribution.Compat.Pty (createPty) where

import System.IO ( Handle )
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
import System.Posix.IO ( fdToHandle )
import System.Posix.Terminal ( openPseudoTerminal )
#else
import Distribution.Compat.CreatePipe
#endif

createPty :: IO (Handle, Handle)
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
createPty = do
    (masterFd, slaveFd) <- openPseudoTerminal
    master <- fdToHandle masterFd
    slave <- fdToHandle slaveFd
    return (master, slave)
#else
createPty = createPipe
#endif
