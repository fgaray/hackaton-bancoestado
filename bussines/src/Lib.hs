module Lib (libMain) where

import FFI.Diagnostic
import FFI.Prolog
import FFI.Python

libMain :: IO ()
libMain = runProlog
