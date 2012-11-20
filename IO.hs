module IO (
io, prompt, Format(..), Only(..), Shown(..), right, left, liftIO
)  where 

import Control.Monad.IO.Class
import Data.Text.Format as Fmt
import Data.Maybe (listToMaybe)

io fmt = liftIO . Fmt.print fmt

prompt fmt args = liftIO loop
  where loop = do
          Fmt.print fmt args
          x <- fmap (listToMaybe . fmap fst . reads) getLine
          case x of
            Nothing -> loop
            Just x' -> return x'
