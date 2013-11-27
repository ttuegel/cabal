-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test.Types
-- Copyright   :  Thomas Tuegel 2013
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Types for the Simple build system's test suite runner.

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

module Distribution.Simple.Test.Types where

import Distribution.Package ( PackageId )
import Distribution.Simple.Compiler ( CompilerId )
import Distribution.System ( Platform )
import Distribution.TestSuite ( Options, Result(..) )

-- | Logs test suite results, itemized by test case.
data TestSuiteLog = TestSuiteLog
    { testSuiteName :: String
    , testLogs :: TestLogs
    , logFile :: FilePath    -- path to human-readable log file
    }
    deriving (Read, Show, Eq)

data TestLogs
    = TestLog
        { testName              :: String
        , testOptionsReturned   :: Options
        , testResult            :: Result
        }
    | GroupLogs String [TestLogs]
    deriving (Read, Show, Eq)

-- | Count the number of pass, fail, and error test results in a 'TestLogs'
-- tree.
countTestResults :: TestLogs
                 -> (Int, Int, Int) -- ^ Passes, fails, and errors,
                                    -- respectively.
countTestResults = go (0, 0, 0)
  where
    go (p, f, e) (TestLog { testResult = r }) =
        case r of
            Pass -> (p + 1, f, e)
            Fail _ -> (p, f + 1, e)
            Error _ -> (p, f, e + 1)
    go (p, f, e) (GroupLogs _ ts) = foldl go (p, f, e) ts

-- | From a 'TestSuiteLog', determine if the test suite passed.
suitePassed :: TestSuiteLog -> Bool
suitePassed l =
    case countTestResults (testLogs l) of
        (_, 0, 0) -> True
        _ -> False

-- | From a 'TestSuiteLog', determine if the test suite failed.
suiteFailed :: TestSuiteLog -> Bool
suiteFailed l =
    case countTestResults (testLogs l) of
        (_, 0, _) -> False
        _ -> True

-- | From a 'TestSuiteLog', determine if the test suite encountered errors.
suiteError :: TestSuiteLog -> Bool
suiteError l =
    case countTestResults (testLogs l) of
        (_, _, 0) -> False
        _ -> True

-- | Logs all test results for a package, broken down first by test suite and
-- then by test case.
data PackageLog = PackageLog
    { package :: PackageId
    , compiler :: CompilerId
    , platform :: Platform
    , testSuites :: [TestSuiteLog]
    }
    deriving (Read, Show, Eq)
