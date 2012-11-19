module IO (
io, Format(..), Only(..), Shown(..), liftIO
)  where 

import Control.Monad.IO.Class
import Data.Text.Format as Fmt

io fmt = liftIO . Fmt.print fmt
