-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test.Utils
-- Copyright   :  Thomas Tuegel 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Common utilities for running test suites.

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

module Distribution.Simple.Test.Utils
    ( resultString
    , summarizePackage
    , summarizeSuiteFinish
    , summarizeSuiteStart
    , testOption
    ) where

import Data.Char ( toUpper )

import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Test.Types
import Distribution.Simple.Utils ( notice )
import Distribution.Verbosity ( Verbosity )

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result
-- isn't neccesarily a path.
testOption :: PD.PackageDescription
           -> LBI.LocalBuildInfo
           -> PD.TestSuite
           -> PathTemplate
           -> String
testOption pkg_descr lbi suite template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)
          (LBI.hostPlatform lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ PD.testName suite)]

-- | Print a summary to the console after all test suites have been run
-- indicating the number of successful test suites and cases.  Returns 'True' if
-- all test suites passed and 'False' otherwise.
summarizePackage :: Verbosity -> PackageLog -> IO Bool
summarizePackage verbosity packageLog = do
    let counts = map (countTestResults . testLogs) $ testSuites packageLog
        (passed, failed, errors) = foldl1 addTriple counts
        totalCases = passed + failed + errors
        passedSuites = length $ filter suitePassed $ testSuites packageLog
        totalSuites = length $ testSuites packageLog
    notice verbosity $ show passedSuites ++ " of " ++ show totalSuites
        ++ " test suites (" ++ show passed ++ " of "
        ++ show totalCases ++ " test cases) passed."
    return $! passedSuites == totalSuites
  where
    addTriple (p1, f1, e1) (p2, f2, e2) = (p1 + p2, f1 + f2, e1 + e2)

-- | Print a summary of the test suite's results on the console, suppressing
-- output for certain verbosity or test filter levels.
summarizeSuiteFinish :: TestSuiteLog -> String
summarizeSuiteFinish testLog = unlines
    [ "Test suite " ++ testSuiteName testLog ++ ": " ++ resStr
    , "Test suite logged to: " ++ logFile testLog
    ]
    where resStr = map toUpper (resultString testLog)

summarizeSuiteStart :: String -> String
summarizeSuiteStart n = "Test suite " ++ n ++ ": RUNNING...\n"

resultString :: TestSuiteLog -> String
resultString l | suiteError l = "error"
               | suiteFailed l = "fail"
               | otherwise = "pass"
