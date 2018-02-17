{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions to run python programs from haskell
module FFI.Python where

import qualified Language.C.Inline as C
import Foreign.C.Types

C.include "<python2.7/Python.h>"


runPython :: IO ()
runPython = [C.block| void {
        Py_Initialize();
    }|]
