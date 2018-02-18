{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Functions to run python programs from haskell
module FFI.Utils (Array(..), withNewArray, withNewArrayFns) where

import qualified Language.C.Inline as C
import GHC.Ptr (Ptr)
import Foreign.Ptr
import Foreign.C.Types
import Import

C.context (C.baseCtx <> C.funCtx) -- Required for function pointers
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
withNewArray ptrs = withNewArrayFns storeAtIndex (length ptrs) ptrs

withNewArrayFns :: (Array -> a -> CInt -> IO ()) -> Int -> [a] -> (Array -> IO b) -> IO b
withNewArrayFns storeAt size ptrs fn = do
    array <- createArray (fromIntegral size)
    mapM_ (\(i, ptr) -> storeAt array ptr (fromIntegral i)) (zip [0..] ptrs)
    result <- fn array
    freeArray array
    return result
