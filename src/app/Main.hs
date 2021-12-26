module Main where

import qualified Lib
import System.Console.CmdArgs
import Args

runDns :: Args
runDns =
  ArgsServer
    { cache_length = 10
    , monitor_domain = Nothing
    } &= name "dns"

mode :: Mode (CmdArgs Args)
mode = cmdArgsMode $ modes [runDns]
  &= help "DNS Flare"
  &= program "dnsflare"
  &= summary "\nv0.1.0, 2021 ROPWARE, Joshua Brown"

main :: IO ()
main = do
  opts <- cmdArgsRun mode
  Lib.main opts

