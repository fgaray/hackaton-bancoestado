{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions to test if the FFI is working. Also, it serves as an example
module FFI.Diagnostic where

import qualified Language.C.Inline as C
import Foreign.C.Types


C.include "<stdio.h>"
C.include "<math.h>"


chello :: IO ()
chello = [C.block| void {
        printf("Hello world from C!\n");
    }|]


cpow :: CDouble -> IO ()
cpow n = [C.block| void {
        double p = pow($(double n), 2);
        printf("Pow: %f\n", p);
    }|]
