import System.Environment
import System.Process
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent
import Text.Read
import Language.Haskell.TH (bang)

forceGetContents :: String -> IO ()
forceGetContents s = void $ evaluate $ length s

-- This is a wrapper program for downpour sybil
-- Example:
-- runghc sybil_wrapper.hs
--      downpour
--      ../downpour/playground/torrents/text.torrent
--      playground/config/
--      playground/data/
--      test.img.text 0 2
main :: IO ()
main = do
    args <- getArgs
    case args of
        [dpath,torrent,conf,outdir,outname,upload,numids] -> do
            case readMaybe numids :: Maybe Integer of
                Just n -> do
                    -- Mount Sybil
                    let downpour = "./" ++ dpath
                        ids  = [0..n-1]
                        args = [[ "dd"
                                , "--torrent"
                                , torrent
                                , "--config"
                                , conf ++ "/config." ++ show id ++ ".yaml"
                                , "--outdir"
                                , outdir ++ "/" ++ show id
                                , "--uploadlimit"
                                , show upload''
                                , "--numidentity"
                                , numids
                                , "--identity"
                                , show id ] | let Just upload' = readMaybe upload :: Maybe Float,
                                              let n' = fromInteger n :: Float,
                                              let upload'' = ceiling $ upload' / n',
                                              id <- ids]
                    -- FIXME Spawning many processes may cause many Sybil
                    -- identity to connect each other. Although they won't
                    -- possibly exchange pieces, inner connections hinder
                    -- them to connect other peers. A workaround is to ban
                    -- the local IP address (either in downpour or config),
                    -- which disables the local test.
                    hs <- forM args (\a -> do
                        (_, Just hout, Just herr, h) <-
                            createProcess (proc downpour a) {
                                  std_out = CreatePipe
                                , std_err = CreatePipe
                            }
                        forkIO $ hGetContents herr >>= forceGetContents
                        forkIO $ hGetContents hout >>= forceGetContents
                        -- forkIO $ hGetContents herr >>= putStrLn
                        -- forkIO $ hGetContents hout >>= putStrLn
                        return h)
                    mapM_ waitForProcess hs
                    -- Collect results
                    let outfiles = [outdir ++ "/" ++ show id ++ "/" ++
                                    outname ++ "." ++ show id | id <- ids]
                    (_, Just hout, _, h) <-
                            createProcess (proc "cat" outfiles) {
                                std_out = CreatePipe
                            }
                    -- FIXME Need to know the outfile name.
                    -- Alternatively, we can read the torrent file for the name.
                    forkIO $ hGetContents hout >>= writeFile outname
                    void $ waitForProcess h
                Nothing -> return ()
        _wrongNumArgs ->
            putStrLn "Usage: \
                     \sybil_wrapper </path/to/downpour> </path/to/torrent> \
                     \</path/to/conf> </path/to/outdir> <outname> <uploadlimit> <numidentities>"
