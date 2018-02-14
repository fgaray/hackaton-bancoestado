{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module FFI.Python where


import qualified Language.C.Inline as C

C.include "<Python.h>"



pythonRunString :: IO ()
pythonRunString = [C.block| void {
        Py_Initialize();
        PyRun_SimpleString("print 'hola mundo'");
        Py_Finalize();
    }|]
