module PythonFFISpec where

import Test.Hspec
import FFI.Python
import FFI.PythonExposed
import Import


spec :: Spec
spec = do
    describe "Python FFI" $ do
        it "can run python programs using variadic" $ withPython $ do
            let n1 :: Int = 1
                n2 :: Int = 2
            result :: Either ByteString Int <- runPython (PythonProgram "prueba" "sum" "test") n1 n2
            liftIO $ result `shouldBe` Right 3

        {-it "should contain the module generated" $ withPython $ do-}
            {-liftIO $ exportFunctions [printFromHaskell]-}
            {-let n1 :: Int = 1-}
            {-result :: Either ByteString ByteString <- runPython (PythonProgram "prueba" "checkHaskellModule" "test") n1-}
            {-liftIO $ result `shouldBe` Right "['__doc__', '__name__', '__package__', 'printFromHaskell']"-}

        it "can access functions from python" $ withPython $ do
            liftIO $ exportFunctions
            let n1 :: Int = 1
                n2 :: Int = 2
            result :: Either ByteString Int <- runPython (PythonProgram "prueba" "callHaskell" "test") n1 n2
            liftIO $ result `shouldBe` Right 3
