module Test.Util
    ( posixToSystemFp
    ) where

import qualified Path as P
import qualified System.FilePath as FP

import Fixtures (fpRoot)

posixToSystemFp :: FilePath -> FilePath
posixToSystemFp posixFp = maybeSystemRoot ++ systemFpRootless
    where
      maybeSystemRoot = if head posixFp == '/' then P.toFilePath fpRoot else ""
      posixFpRootless = if head posixFp == '/' then tail posixFp else posixFp
      systemFpRootless = map (\c -> if c == '/' then FP.pathSeparator else c) posixFpRootless
