{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions to run prolog queries over databases
module FFI.Prolog where

import qualified Language.C.Inline as C
import Foreign.C.Types

C.include "<stdio.h>"
C.include "/usr/lib/swi-prolog/include/SWI-Prolog.h"



runProlog :: IO ()
runProlog = [C.block| void {

        char *av[10];
        int ac = 0;

        av[ac++] = "bussines";
        av[ac++] = "-x";
        av[ac++] = "mystate";
        av[ac]   = NULL;

        if (!PL_initialise(ac, av)) {
            printf("Error initializing prolog\n");
            return;
        }
    }|]
