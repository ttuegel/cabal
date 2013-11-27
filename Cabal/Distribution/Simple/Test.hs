-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into testing a built package. It performs the
-- \"@.\/setup test@\" action. It runs test suites designated in the package
-- description and reports on the results.

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

module Distribution.Simple.Test
    ( test
    ) where

import Control.Monad ( when, unless, filterM )
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getDirectoryContents
    , removeFile )
import System.Exit ( ExitCode(..), exitFailure, exitWith )
import System.FilePath ( (</>) )

import qualified Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(buildable)
         , TestSuite(..)
         , TestSuiteInterface(..), testType, hasTests )
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.Hpc ( markupPackage )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
    ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), fromFlag )
import qualified Distribution.Simple.Test.ExeV10 as ExeV10
import qualified Distribution.Simple.Test.LibV09 as LibV09
import Distribution.Simple.Test.Types
import Distribution.Simple.Test.Utils
import Distribution.Simple.Utils ( die, notice )
import Distribution.TestSuite ( Result(..) )
import Distribution.Text

-- | A 'PackageLog' with package and platform information specified.
localPackageLog :: PD.PackageDescription -> LBI.LocalBuildInfo -> PackageLog
localPackageLog pkg_descr lbi = PackageLog
    { package = PD.package pkg_descr
    , compiler = compilerId $ LBI.compiler lbi
    , platform = LBI.hostPlatform lbi
    , testSuites = []
    }

-- |Perform the \"@.\/setup test@\" action.
test :: PD.PackageDescription   -- ^information from the .cabal file
     -> LBI.LocalBuildInfo      -- ^information from the configure step
     -> TestFlags               -- ^flags sent to test
     -> IO ()
test pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        humanTemplate = fromFlag $ testHumanLog flags
        machineTemplate = fromFlag $ testMachineLog flags
        distPref = fromFlag $ testDistPref flags
        testLogDir = distPref </> "test"
        testNames = fromFlag $ testList flags
        pkgTests = PD.testSuites pkg_descr
        enabledTests = [ t | t <- pkgTests
                           , PD.testEnabled t
                           , PD.buildable (PD.testBuildInfo t) ]

        doTest :: (PD.TestSuite, Maybe TestSuiteLog) -> IO TestSuiteLog
        doTest (suite, _) = do
            let testLogPath = testSuiteLogPath humanTemplate pkg_descr lbi
            case PD.testInterface suite of
              PD.TestSuiteExeV10 _ _ ->
                  ExeV10.runTest flags pkg_descr lbi suite testLogPath

              PD.TestSuiteLibV09 _ _ ->
                  LibV09.runTest flags pkg_descr lbi suite testLogPath

              _ -> return TestSuiteLog
                            { testSuiteName = PD.testName suite
                            , testLogs = TestLog
                                { testName = PD.testName suite
                                , testOptionsReturned = []
                                , testResult = Error $
                                    "No support for running test suite type: "
                                    ++ show (disp $ PD.testType suite)
                                }
                            , logFile = ""
                            }

    when (not $ PD.hasTests pkg_descr) $ do
        notice verbosity "Package has no test suites."
        exitWith ExitSuccess

    when (PD.hasTests pkg_descr && null enabledTests) $
        die $ "No test suites enabled. Did you remember to configure with "
              ++ "\'--enable-tests\'?"

    testsToRun <- case testNames of
            [] -> return $ zip enabledTests $ repeat Nothing
            names -> flip mapM names $ \tName ->
                let testMap = zip enabledNames enabledTests
                    enabledNames = map PD.testName enabledTests
                    allNames = map PD.testName pkgTests
                in case lookup tName testMap of
                    Just t -> return (t, Nothing)
                    _ | tName `elem` allNames ->
                          die $ "Package configured with test suite "
                                ++ tName ++ " disabled."
                      | otherwise -> die $ "no such test: " ++ tName

    createDirectoryIfMissing True testLogDir

    -- Delete ordinary files from test log directory.
    getDirectoryContents testLogDir
        >>= filterM doesFileExist . map (testLogDir </>)
        >>= mapM_ removeFile

    let totalSuites = length testsToRun
    notice verbosity $ "Running " ++ show totalSuites ++ " test suites..."
    suites <- mapM doTest testsToRun
    let packageLog = (localPackageLog pkg_descr lbi) { testSuites = suites }
        packageLogFile = (</>) testLogDir
            $ packageLogPath machineTemplate pkg_descr lbi
    allOk <- summarizePackage verbosity packageLog
    writeFile packageLogFile $ show packageLog

    markupPackage verbosity lbi distPref (display $ PD.package pkg_descr)
        $ map fst testsToRun

    unless allOk exitFailure

testSuiteLogPath :: PathTemplate
                 -> PD.PackageDescription
                 -> LBI.LocalBuildInfo
                 -> TestSuiteLog
                 -> FilePath
testSuiteLogPath template pkg_descr lbi testLog =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)
                (LBI.hostPlatform lbi)
                ++  [ (TestSuiteNameVar, toPathTemplate $ testSuiteName testLog)
                    , (TestSuiteResultVar, result)
                    ]
        result = toPathTemplate $ resultString testLog

packageLogPath :: PathTemplate
               -> PD.PackageDescription
               -> LBI.LocalBuildInfo
               -> FilePath
packageLogPath template pkg_descr lbi =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)
                (LBI.hostPlatform lbi)
