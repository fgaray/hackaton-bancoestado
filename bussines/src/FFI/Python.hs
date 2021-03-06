{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions to run python programs from haskell
module FFI.Python where

import qualified Language.C.Inline as C
import Foreign.C.Types
import GHC.Ptr (Ptr, nullPtr)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import FFI.Utils
import Control.Monad (void)
import Import
import System.Environment (getProgName)

C.include "<python2.7/Python.h>"


-- | Represents a FFI Pointer to a python object
newtype PythonPtr a = PythonPtr { unPythonPtr :: (Ptr ()) }

-- | To force the user to use the withPython function to get inside a
-- initialized python enviroment
newtype PythonMonad a = PythonMonad { unPythonMonad :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

data PythonNone = PythonNone

-- | Different kinds of arguments from haskell to Python object
class PythonArg a where
    toPython    :: a -> PythonMonad (PythonPtr a)
    fromPython  :: (PythonPtr a) -> PythonMonad a

    -- default. Frees the memory of the python object
    cleanPython :: PythonPtr a -> PythonMonad ()
    cleanPython (PythonPtr ptr) = liftIO $ [C.block| void {
         Py_DECREF($(void * ptr));
    }|]

instance PythonArg Int where
    toPython n     = fmap (PythonPtr . unPythonPtr) $ toPython ((fromIntegral n) :: CLong)
    fromPython (PythonPtr ptr) = ((fromPython (PythonPtr ptr)) :: PythonMonad CLong) >>= return . fromIntegral

instance PythonArg CLong where
    toPython :: CLong -> PythonMonad (PythonPtr CLong)
    toPython n = fmap PythonPtr . liftIO $ [C.block| void * {
       PyObject *number = PyInt_FromLong($(long n));
       return number;
    }|]

    fromPython :: (PythonPtr CLong) -> PythonMonad CLong
    fromPython (PythonPtr ptr) = liftIO [C.block| long {
        PyInt_AsLong($(void * ptr));
    }|]


instance PythonArg ByteString where
    toPython :: ByteString -> PythonMonad (PythonPtr ByteString)
    toPython bs = fmap PythonPtr $ liftIO $ BS.useAsCString bs (\str -> [C.block| void * {
        return PyString_FromString($(char * str));
    }|])
    
    
    fromPython :: (PythonPtr ByteString) -> PythonMonad ByteString
    fromPython (PythonPtr ptr) = liftIO $ do
        cstr <- [C.block| char* {
            return PyString_AsString($(void *ptr));
        }|]
        BS.packCString cstr

instance PythonArg PythonNone where
    toPython PythonNone = fmap PythonPtr . liftIO $ [C.block| void * {
        return Py_BuildValue("");
    }|]

    fromPython = undefined



-- | Class for variadic argument in the calling function for python
class PythonRunType t where
    runPython' :: PythonProgram -> [PythonMonad (Ptr ())] -> t

-- | Tail instance for the recursive variadic function
instance PythonArg a => PythonRunType (PythonMonad (Either ByteString a)) where
    runPython' :: PythonProgram -> [PythonMonad (Ptr ())] -> PythonMonad (Either ByteString a)
    runPython' program ptrs = do
        ptrs'  <- mapM id ptrs
        result <- liftIO $ runPythonPtrs program ptrs'
        mapM cleanPythonPtr ptrs' -- Clean all the created ptrs
        convert result
        where
            convert :: Either ByteString (Ptr ()) -> PythonMonad (Either ByteString a)
            convert (Right ptr) = do
                let py :: PythonPtr a = PythonPtr ptr
                value <- fromPython py
                cleanPython py
                return (Right value)
            convert (Left err) = return $ Left err

-- | This instance collects all the arguments of python from haskell
instance (PythonArg a, PythonRunType r) => PythonRunType (a -> r) where
    runPython' :: PythonProgram -> [PythonMonad (Ptr ())] -> (a -> r)
    runPython' program acc = \ptr -> runPython' program (acc ++ [fmap unPythonPtr $ toPython ptr])



data PythonProgram = PythonProgram
    { pythonProgramModule       :: ByteString
    , pythonProgramFunction     :: ByteString
    , pythonProgramFolder       :: ByteString -- | Where the .py module is
    } deriving Show


withPython :: (PythonMonad ()) -> IO ()
withPython fn = do
    [C.block| void { Py_Initialize(); }|]
    unPythonMonad fn
    [C.block| void { Py_Finalize(); }|]


-- | Generic variadic function to call a python program. Caller does not need to
-- convert the returned value, that is handled by the class PythonRunType
runPython :: PythonRunType a => PythonProgram -> a
runPython prog = runPython' prog []


-- | Tells python to use the given directory to search for modules to be
-- imported
setupPath :: ByteString -> PythonMonad ()
setupPath path = do
    name <- liftIO $ fmap (BS.pack) getProgName

    liftIO $
        BS.useAsCString path $ \path' ->
        BS.useAsCString name $ \name' -> do
            [C.block| void {
                PyObject *current;
                PyObject *sysPath = PySys_GetObject((char*)"path");

                current = PyString_FromString($(char *name'));
                PyList_Append(sysPath, current);
                Py_DECREF(current);

                current = PyString_FromString($(char *path'));
                PyList_Append(sysPath, current);
                Py_DECREF(current);
            }|]


runPythonPtrs :: PythonProgram -> [Ptr ()] -> IO (Either ByteString (Ptr ()))
runPythonPtrs PythonProgram{..} ptrs = BS.useAsCString pythonProgramModule $ \moduleName -> BS.useAsCString pythonProgramFunction $ \functionName -> do
    unPythonMonad $ setupPath pythonProgramFolder
    let ptrsSize = fromIntegral $ length ptrs
    result <- withNewArray ptrs $ \(Array array) ->
        [C.block| void * {
            PyErr_Clear();
            void **ptrs = $(void* array);

            PyObject *pName = PyString_FromString($(char* moduleName));
            PyObject *pModule = PyImport_Import(pName);
            if (!pModule) {
                printf("PYTHON: Can't import module %s\n", $(char* moduleName));
                return NULL;
            }

            PyObject *pFunc = PyObject_GetAttrString(pModule, $(char* functionName));
            if (!pFunc) {
                printf("PYTHON: Function %s not found\n", $(char* functionName));
                PyObject_Print(pModule, stdout, Py_PRINT_RAW);
                return NULL;
            }

            PyObject *pArgs = PyTuple_New($(int ptrsSize));

            for (int i = 0; i < $(int ptrsSize); ++i) {
              PyTuple_SetItem(pArgs, i, ptrs[i]);
            }

            PyObject *result = PyObject_CallObject(pFunc, pArgs);


            // Clean
            Py_DECREF(pFunc);
            Py_DECREF(pName);
            Py_DECREF(pArgs);

            return result;
        }|]
    if result /= nullPtr
        then return (Right result)
        else do
            -- get the error
            err <- unPythonMonad getError
            case err of
                Nothing -> return $ Left "Can't get the error from python"
                Just err' -> return $ Left err'


getError :: PythonMonad (Maybe ByteString)
getError = do
    err <- liftIO $ [C.block| void * {
        PyObject *ptype, *pvalue, *ptrace;
        PyErr_Fetch(&ptype, &pvalue, &ptrace);

        if (!ptype) {
            // there are no exceptions
            return NULL;
        }

        //pvalue contains error message
        //ptraceback contains stack snapshot and many other information
        //(see python traceback structure)
        return pvalue;
        }|]
    if err /= nullPtr
        then do
            bs <- fromPython (PythonPtr err)
            return (Just bs)
        else return Nothing

cleanPythonPtr :: Ptr () -> PythonMonad ()
cleanPythonPtr ptr = liftIO [C.block| void { Py_DECREF($(void * ptr)); } |]

