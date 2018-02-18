{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions that python is allowed to access. This module should not be
-- imported from any other part that the init function of the main program
module FFI.PythonExposed where

import qualified Language.C.Inline as C
import Foreign.C.Types
import Import
import FFI.Python
import Foreign.Ptr
import FFI.Utils


C.context (C.baseCtx <> C.funCtx) -- Required for function pointers
C.include "<python2.7/Python.h>"
C.include "<malloc.h>"



exportFunctions :: IO ()
exportFunctions = [C.block| void {

    extern PyObject *python_c_wrapper(PyObject *self, PyObject *args);
    extern void python_c_set_function(void* (*fun)(void*, void*));

    python_c_set_function($fun:(void* (*haskellDispatch)(void*, void*)));

    static PyMethodDef methods[] = {
        {"call", python_c_wrapper, METH_VARARGS, "The first argument is the name of the function to call, the second is the arguments of that function"},
        {NULL, NULL, 0, NULL}
        };

    Py_InitModule("haskell", methods);

    }|]


foreign export ccall 
    haskellDispatch :: Ptr () -> Ptr () -> IO (Ptr ())

haskellDispatch _ _ = do
    putStrLn "Hola mundo"
    return nullPtr
        {-ptr <- unPythonMonad . toPython $ PythonNone-}
        {-return (unPythonPtr ptr)-}
