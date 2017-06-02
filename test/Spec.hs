import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.Megaparsec
import Config

main :: IO ()
main = hspec $ do
    describe "Config.cmdP" $ do
        it "should use the whole dir as input" $ do
            runParser cmdP "" "./../examples --tall --dir" `shouldBe` (Right (CmdParam {path = "./../examples", typeErrLvl' = AllErrors, wholeDir = True}))

        it "should only use the defined Java file as input" $ do
            runParser cmdP "" "./../examples/Add.java --tall" `shouldBe` (Right (CmdParam {path = "./../examples/Add.java", typeErrLvl' = AllErrors, wholeDir = False}))

        it "should use the whole dir as input" $ do
            runParser cmdP "" "examples/z_ShouldFail --tall --dir" `shouldBe` (Right (CmdParam {path = "examples/z_ShouldFail", typeErrLvl' = AllErrors, wholeDir = True}))

        it "should use the whole dir as input" $ do
            runParser cmdP "" ".././examples/z_ShouldFail/ParseError --tall --dir" `shouldBe` (Right (CmdParam {path = ".././examples/z_ShouldFail/ParseError", typeErrLvl' = AllErrors, wholeDir = True}))

        it "should take tall as typeErrLevel" $ do
            runParser cmdP "" ". --tall" `shouldBe` (Right (CmdParam {path = ".", typeErrLvl' = AllErrors, wholeDir = False}))

        it "should take tfirst as typeErrLevel" $ do
            runParser cmdP "" ". --tfirst" `shouldBe` (Right (CmdParam {path = ".", typeErrLvl' = FirstError, wholeDir = False}))

        it "should take tsilent as typeErrLevel" $ do
            runParser cmdP "" ". --tsilent" `shouldBe` (Right (CmdParam {path = ".", typeErrLvl' = Silently, wholeDir = False}))
