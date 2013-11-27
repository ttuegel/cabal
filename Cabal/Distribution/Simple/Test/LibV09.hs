-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test.LibV09
-- Copyright   :  Thomas Tuegel 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module runs test suites conforming to TestSuiteLibV09
-- (detailed-0.9) interface.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Test.LibV09
    ( realMain
    , runTest
    , stubFilePath
    , stubName
    , writeStubSource
    ) where

import Distribution.Compat.Environment ( getEnvironment )
import Distribution.Compat.TempFile ( openTempFile )
import Distribution.ModuleName ( ModuleName )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build.PathsModule ( pkgPathEnvVar )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Hpc
    ( markupTest, tixDir, tixFilePath )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup
    ( TestFlags(..), TestShowDetails(..), fromFlag )
import Distribution.Simple.Test.Types
import Distribution.Simple.Test.Utils
import Distribution.Simple.Utils ( die, notice, rawSystemIOWithEnv )
import Distribution.TestSuite
import Distribution.Text
import Distribution.Verbosity ( normal, Verbosity )

import Control.Exception ( bracket )
import Control.Monad ( unless, when )
import Data.Maybe ( mapMaybe )
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive, removeFile
    , setCurrentDirectory )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose, IOMode(..), openFile )

-- Test runner ---------------------------------------------------------

runTest :: TestFlags
        -- ^ flags Cabal was invoked with
        -> PD.PackageDescription
        -- ^ description of package the test suite belongs to
        -> LBI.LocalBuildInfo
        -- ^ information from the configure step
        -> PD.TestSuite
        -- ^ TestSuite being tested
        -> (TestSuiteLog -> FilePath)
        -- ^ generator for final human-readable log filename
        -> IO TestSuiteLog
runTest flags pkgDescr lbi suite testLogPath = do
    let cmd = LBI.buildDir lbi
            </> PD.testName suite
            </> PD.testName suite
            <.> exeExtension
        preTest _ = ""
        postTest exit _ =
            TestSuiteLog
                { testSuiteName = PD.testName suite
                , testLogs = TestLog
                    { testName = PD.testName suite
                    , testOptionsReturned = []
                    , testResult = case exit of
                        ExitSuccess -> Pass
                        ExitFailure c -> Fail $ "exit code: " ++ show c
                    }
                , logFile = ""
                }
    testController flags pkgDescr lbi suite preTest cmd postTest testLogPath

-- | Run a test executable, logging the output and generating the appropriate
-- summary messages.
testController :: TestFlags
               -- ^ flags Cabal was invoked with
               -> PD.PackageDescription
               -- ^ description of package the test suite belongs to
               -> LBI.LocalBuildInfo
               -- ^ information from the configure step
               -> PD.TestSuite
               -- ^ TestSuite being tested
               -> (FilePath -> String)
               -- ^ prepare standard input for test executable
               -> FilePath -- ^ executable name
               -> (ExitCode -> String -> TestSuiteLog)
               -- ^ generator for the TestSuiteLog
               -> (TestSuiteLog -> FilePath)
               -- ^ generator for final human-readable log filename
               -> IO TestSuiteLog
testController flags pkg_descr lbi suite preTest cmd postTest logNamer = do
    let distPref = fromFlag $ testDistPref flags
        verbosity = fromFlag $ testVerbosity flags
        testLogDir = distPref </> "test"
        opts = map (testOption pkg_descr lbi suite) $ testOptions flags

    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment
    let dataDirPath = pwd </> PD.dataDir pkg_descr
        shellEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                   : ("HPCTIXFILE", (</>) pwd
                       $ tixFilePath distPref $ PD.testName suite)
                   : existingEnv

    bracket (openCabalTemp testLogDir) deleteIfExists $ \tempLog ->
        bracket (openCabalTemp testLogDir) deleteIfExists $ \tempInput -> do

            -- Check that the test executable exists.
            exists <- doesFileExist cmd
            unless exists $ die $ "Error: Could not find test program \"" ++ cmd
                                  ++ "\". Did you build the package first?"

            -- Remove old .tix files if appropriate.
            unless (fromFlag $ testKeepTix flags) $ do
                let tDir = tixDir distPref $ PD.testName suite
                exists' <- doesDirectoryExist tDir
                when exists' $ removeDirectoryRecursive tDir

            -- Create directory for HPC files.
            createDirectoryIfMissing True $ tixDir distPref $ PD.testName suite

            -- Write summary notices indicating start of test suite
            notice verbosity $ summarizeSuiteStart $ PD.testName suite

            -- Prepare standard input for test executable
            appendFile tempInput $ preTest tempInput

            -- Run test executable
            exit <- do
              hLog <- openFile tempLog AppendMode
              hIn  <- openFile tempInput ReadMode
              -- these handles get closed by rawSystemIOWithEnv
              rawSystemIOWithEnv verbosity cmd opts Nothing (Just shellEnv)
                                 (Just hIn) (Just hLog) (Just hLog)

            -- Generate TestSuiteLog from executable exit code and a machine-
            -- readable test log
            suiteLog <- fmap (postTest exit $!) $ readFile tempInput

            -- Generate final log file name
            let finalLogName = testLogDir </> logNamer suiteLog
                suiteLog' = suiteLog { logFile = finalLogName }

            -- Write summary notice to log file indicating start of test suite
            appendFile (logFile suiteLog') $ summarizeSuiteStart $ PD.testName suite

            -- Append contents of temporary log file to the final human-
            -- readable log file
            readFile tempLog >>= appendFile (logFile suiteLog')

            -- Write end-of-suite summary notice to log file
            appendFile (logFile suiteLog') $ summarizeSuiteFinish suiteLog'

            -- Show the contents of the human-readable log file on the terminal
            -- if there is a failure and/or detailed output is requested
            let details = fromFlag $ testShowDetails flags
                whenPrinting = when $ (details > Never)
                    && (not (suitePassed suiteLog) || details == Always)
                    && verbosity >= normal
            whenPrinting $ readFile tempLog >>=
                putStr . unlines . lines

            -- Write summary notice to terminal indicating end of test suite
            notice verbosity $ summarizeSuiteFinish suiteLog'

            markupTest verbosity lbi distPref
                (display $ PD.package pkg_descr) suite

            return suiteLog'
    where
        deleteIfExists file = do
            exists <- doesFileExist file
            when exists $ removeFile file

        openCabalTemp testLogDir = do
            (f, h) <- openTempFile testLogDir $ "cabal-test-" <.> "log"
            hClose h >> return f

-- Utilities -----------------------------------------------------------

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: PD.TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: PD.TestSuite -> FilePath
stubName t = PD.testName t ++ "Stub"

-- | Write the source file for a library 'TestSuite' stub executable.
writeStubSource :: PD.TestSuite -- ^ library 'TestSuite' for which a stub
                                -- is being created
                -> FilePath     -- ^ path to directory where stub source
                                -- should be located
                -> IO ()
writeStubSource t dir = do
    createDirectoryIfMissing True dir
    let filename = dir </> stubFilePath t
        PD.TestSuiteLibV09 _ m = PD.testInterface t
    writeFile filename $ stubSource m

-- | Source code for library test suite stub executable
stubSource :: ModuleName -> String
stubSource m = unlines
    [ "module Main ( main ) where"
    , "import Distribution.Simple.Test.LibV09 ( realMain )"
    , "import " ++ show (disp m) ++ " ( tests )"
    , "main :: IO ()"
    , "main = realMain tests"
    ]

-- Test stub executable ------------------------------------------------

-- | Main function for test stubs. Once, it was written directly into the stub,
-- but minimizing the amount of code actually in the stub maximizes the number
-- of detectable errors when Cabal is compiled.
realMain :: IO [Test] -> IO ()
realMain tests = do
    (f, n) <- fmap read getContents
    dir <- getCurrentDirectory
    results <- tests >>= stubRunTests
    setCurrentDirectory dir
    stubWriteLog f n results

-- | The test runner used in library "TestSuite" stub executables.  Runs a list
-- of 'Test's.  An executable calling this function is meant to be invoked as
-- the child of a Cabal process during @.\/setup test@.  A 'TestSuiteLog',
-- provided by Cabal, is read from the standard input; it supplies the name of
-- the test suite and the location of the machine-readable test suite log file.
-- Human-readable log information is written to the standard output for capture
-- by the calling Cabal process.
stubRunTests :: [Test] -> IO TestLogs
stubRunTests tests = do
    logs <- mapM stubRunTests' tests
    return $ GroupLogs "Default" logs
  where
    stubRunTests' (Test t) = do
        l <- run t >>= finish
        summarizeTest normal Always l
        return l
      where
        finish (Finished result) =
            return TestLog
                { testName = name t
                , testOptionsReturned = defaultOptions t
                , testResult = result
                }
        finish (Progress _ next) = next >>= finish
    stubRunTests' g@(Group {}) = do
        logs <- mapM stubRunTests' $ groupTests g
        return $ GroupLogs (groupName g) logs
    stubRunTests' (ExtraOptions _ t) = stubRunTests' t
    maybeDefaultOption opt =
        maybe Nothing (\d -> Just (optionName opt, d)) $ optionDefault opt
    defaultOptions testInst = mapMaybe maybeDefaultOption $ options testInst

-- | From a test stub, write the 'TestSuiteLog' to temporary file for the calling
-- Cabal process to read.
stubWriteLog :: FilePath -> String -> TestLogs -> IO ()
stubWriteLog f n logs = do
    let testLog = TestSuiteLog { testSuiteName = n, testLogs = logs, logFile = f }
    writeFile (logFile testLog) $ show testLog
    when (suiteError testLog) $ exitWith $ ExitFailure 2
    when (suiteFailed testLog) $ exitWith $ ExitFailure 1
    exitWith ExitSuccess

-- | Print a summary of a single test case's result to the console, supressing
-- output for certain verbosity or test filter levels.
summarizeTest :: Verbosity -> TestShowDetails -> TestLogs -> IO ()
summarizeTest _ _ (GroupLogs {}) = return ()
summarizeTest verbosity details t =
    when shouldPrint $ notice verbosity $ "Test case " ++ testName t
        ++ ": " ++ show (testResult t)
    where shouldPrint = (details > Never) && (notPassed || details == Always)
          notPassed = testResult t /= Pass
