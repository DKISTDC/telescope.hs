{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Notebook where

import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import System.FilePath


newtype Html = Html {text :: Text}


data Notebook = Notebook FilePath [FilePath]


export :: (IOE :> es, Default r, ToRenderable r) => FilePath -> EC r () -> Eff es ()
export f ec = do
  liftIO $ toFile def ("./output/" ++ f) ec


runNotebook :: FilePath -> Eff (Writer [FilePath] : Reader FilePath : es) () -> Eff es Notebook
runNotebook folder eff = do
  fps <- runReader folder $ execWriter eff
  pure $ Notebook folder fps


writeNotebook :: (IOE :> es) => Notebook -> Eff es ()
writeNotebook nb@(Notebook folder _) = do
  let Html out = toHtml nb
  liftIO $ T.writeFile (folder </> "notebook.html") out


toHtml :: Notebook -> Html
toHtml (Notebook _ files) =
  let body = T.intercalate "\n" $ fmap img files
   in Html
        [i|<!DOCTYPE html>
 <html>
 <body>
  #{body}
 </body>
 </html>|]
 where
  img f = [i|<img src="#{f}" width="500"/>|]


newtype CellId = CellId String
  deriving newtype (IsString)


graph :: (Writer [FilePath] :> es, IOE :> es) => CellId -> EC (Layout Double Double) () -> Eff es ()
graph (CellId lbl) ec = do
  let file = lbl <> ".svg"
  export file $ do
    layout_title .= lbl
    ec
  tell [file]
