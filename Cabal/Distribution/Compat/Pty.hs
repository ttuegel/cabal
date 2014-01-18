{-# LANGUAGE CPP #-}
module Distribution.Compat.Pty
    ( PtyAttributes
    , PtyHandle
    , createPty
    , ptyIn, ptyErr, ptyOut
    , ptyToHandle
    , getPtyAttributes, setPtyAttributes
    ) where

import System.IO ( Handle )
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
import System.Posix.IO
import System.Posix.Types ( Fd )
import System.Posix.Terminal
#else
import Distribution.Compat.CreatePipe
import System.IO ( stdin, stderr, stdout )
#endif

type PtyHandle
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
    = Fd
#else
    = Handle
#endif

type PtyAttributes
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
    = TerminalAttributes
#else
    = ()
#endif

ptyIn, ptyOut, ptyErr :: PtyHandle
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
ptyIn = stdInput
ptyOut = stdOutput
ptyErr = stdError
#else
ptyIn = stdin
ptyOut = stdout
ptyErr = stderr
#endif

createPty :: IO (PtyHandle, PtyHandle)
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
createPty = openPseudoTerminal
#else
createPty = createPipe
#endif

getPtyAttributes :: PtyHandle -> IO PtyAttributes
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
getPtyAttributes = getTerminalAttributes
#else
getPtyAttributes _ = return ()
#endif

setPtyAttributes :: PtyHandle -> PtyAttributes -> IO ()
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
setPtyAttributes pty attrs = setTerminalAttributes pty attrs WhenDrained
#else
setPtyAttributes _ _ = return ()
#endif

ptyToHandle :: PtyHandle -> IO Handle
#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
ptyToHandle = fdToHandle
#else
ptyToHandle = id
#endif
