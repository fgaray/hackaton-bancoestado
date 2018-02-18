{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions to run python programs from haskell
module FFI.Utils (Array(..), withNewArray) where

import qualified Language.C.Inline as C
import GHC.Ptr (Ptr)
import Foreign.C.Types
import Control.Monad (mapM_)

C.include "<malloc.h>"

newtype Array = Array { unArray :: (Ptr ()) }

-- | Simple array to store void ptrs for function arguments
createArray :: CInt -> IO Array
createArray size = fmap Array [C.block| void * {
        return malloc(sizeof(void*)*$(int size));
    }|]


storeAtIndex :: Array -> Ptr () -> CInt -> IO ()
storeAtIndex (Array arr) ptr index = [C.block| void {
        void **arr = $(void* arr);
        arr[$(int index)] = $(void * ptr);
    }|]

getAtIndex :: Array -> CInt -> IO (Ptr ())
getAtIndex (Array arr) index = [C.block| void * {
        void **arr = $(void* arr);
        return arr[$(int index)];
    }|]

freeArray :: Array -> IO ()
freeArray (Array arr) = [C.block| void {
        free($(void* arr));
    }|]

withNewArray :: [Ptr ()] -> (Array -> IO a) -> IO a
withNewArray ptrs fn = do
    array <- createArray (fromIntegral $ length ptrs)
    mapM_ (\(i, ptr) -> storeAtIndex array ptr (fromIntegral i)) (zip [0..] ptrs)
    result <- fn array
    freeArray array
    return result
