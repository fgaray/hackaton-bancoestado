module Lib (libMain) where

import FFI.Diagnostic
import FFI.Prolog

libMain :: IO ()
libMain = runProlog
