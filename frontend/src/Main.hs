{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

module Main where

import           Data.Semigroup
import qualified Data.Text.Lazy                as L
import           Reflex.Dom
import           Text.Blaze.Html.Renderer.Text
import           Text.Markdown

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget :: MonadWidget t m => m ()
headWidget = elAttr
  "link"
  (  "rel"
  =: "stylesheet"
  <> "href"
  =: "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
  )
  blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = mdo
  el "h1" $ text "Hello, Haskell!"
  el "h1" $ dynText $ value ti
  _ <-
    elDynHtml'
      "div"
      (L.toStrict . renderHtml . markdown def . L.fromStrict <$> value ta)
  (ti, ta) <- (,) <$> textInput def <*> textArea def
  blank
