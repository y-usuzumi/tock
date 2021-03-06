module Tock.CodeGen.Markdown where

import           Control.Monad
import           Data.Functor.Foldable
import           Data.List
import           Data.Tree
import           Tock.DataTypes
import           Network.URI.Encode
import           Text.Printf

data RenderOptions = RenderOptions { excludeEmptyDirs :: Bool
                                   }

renderToc :: RenderOptions -> Toc -> String
renderToc RenderOptions{..} toc =
  intercalate "\n" $ flip cata toc $ \(NodeF TocItem{..} children) ->
  case type_ of
    Directory ->
      if excludeEmptyDirs && null (join children)
      then []
      else printf "* [%s](%s)" title (encode link) :
           map ("  " ++) (join children)
    File -> [printf "* [%s](%s)" title (encode link)]

renderTocs :: RenderOptions -> [Toc] -> String
renderTocs opts = intercalate "\n" . filter (/= "") . map (renderToc opts)
