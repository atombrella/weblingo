module Config where

import Hakyll (FeedConfiguration(..))

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "monad.dk"
  , feedDescription = "Personal blog of Mads Jensen"
  , feedAuthorName = "Mads Jensen"
  , feedAuthorEmail = "mje@inducks.org"
  , feedRoot = "https://monad.dk"
}
