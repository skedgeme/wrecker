import Wrecker
import Wrecker.Options
import Network.HTTP.Client
import Options.Applicative.Builder
import Options.Applicative
import Data.Monoid
import Control.Exception
import Control.Monad (void)
import Control.Lens

parser :: Parser (PartialOptions, String)
parser
   =  (,)
  <$> pPartialOptions
  <*> strArgument mempty

runParser' :: IO (Options, String)
runParser' = do
  let opts = info (helper <*> parser)
               ( fullDesc
               <> progDesc "Welcome to wrecker"
               <> header "wrecker - HTTP stress tester and benchmarker"
               )

  (partialOptions, url) <- execParser opts
  options <- case completeOptions partialOptions of
               Nothing -> throwIO $ userError ""
               Just x  -> return x
  return (options, url)

main :: IO ()
main = do
  (options, url) <- runParser'
{-
  man <- newManager defaultManagerSettings { managerConnCount           = concurrency options
                                           , managerIdleConnectionCount = concurrency options
                                           }
  void $ runOne options $ \env ->
    void $ record (recorder env) url
         $ Wreq.getWith (Wreq.defaults & Wreq.manager .~ Right man) url
-}
  man <- newManager defaultManagerSettings { managerConnCount           = concurrency options
                                           , managerIdleConnectionCount = concurrency options
                                           }

  req <- parseRequest url
  void $ runOne options $ \env ->
    void $ record (recorder env) url $ httpLbs req man
