module PythonFFISpec where

import Test.Hspec
import FFI.Python
import Import


spec :: Spec
spec = do
    describe "Python FFI" $ do
        it "can run python programs using variadic" $ withPython $ do
            let n1 :: Int = 1
                n2 :: Int = 2
            result <- runPython (PythonProgram "prueba" "sum" "test") n1 n2
            case result of
                Nothing -> liftIO $ 0 `shouldBe` 3
                Just ptr -> do
                    number :: Int <- fromPython ptr
                    liftIO $ number `shouldBe` 3
                    cleanPython ptr
