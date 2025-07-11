module Test.ParserSpec where

import Data.List (isInfixOf)
import Effectful
import Effectful.Error.Static
import Effectful.NonDet
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Data.Parser


spec :: Spec
spec = do
  describe "Data.Parser" $ do
    describe "parsing" $ withMarkers ["focus"] $ do
      it "parses something" $ do
        res <- runParseTest $ do
          pure "hello"
        res `shouldBe` Right ("hello" :: String)

      it "can handle alternatives" $ do
        let pa = pure ()
        res <- runParseTest pa
        res `shouldBe` Right ()

        let pb = pure ()
        res2 <- runParseTest $ runParserAlts (parseFail "Empty") empty
        res2 `shouldSatisfy` P.left P.anything

        res3 <- runParseTest $ runParserAlts (parseFail "Empty") (empty <|> pb)
        res3 `shouldBe` Right ()

    describe "location " $ do
      it "changes failure to location" $ do
        res <- runParseTest $ do
          parseAt (Child "hello") $ parseAt (Index 2) $ do
            parseFail "nope"

        case res of
          Left (ParseFailure path err) -> do
            path `shouldBe` Path [Child "hello", Index 2]
            err `shouldBe` "nope"
          Right _ -> failTest "Expected failure"

      it "gives good expected messages" $ do
        res :: Either ParseError () <- runParseTest $ do
          parseAt (Child "child") $ parseAt (Index 2) $ expected "Number" (1 :: Int)
        ("child/2" `isInfixOf` show res) `shouldBe` True
        ("1" `isInfixOf` show res) `shouldBe` True
        ("Number" `isInfixOf` show res) `shouldBe` True


runParseTest :: Eff [Parser, Error ParseError, IOE] a -> IO (Either ParseError a)
runParseTest eff = do
  runEff . runErrorNoCallStack @ParseError . runParser $ eff
