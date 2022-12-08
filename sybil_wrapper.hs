import System.Environment
import System.Process
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent
import Text.Read

forceGetContents :: String -> IO ()
forceGetContents s = void $ evaluate $ length s

-- This is a wrapper program for downpour sybil
main :: IO ()
main = do
    args <- getArgs
    case args of
        [dpath,torrent,conf,out,upload,numids] -> do
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
                                , out ++ "/" ++ show id
                                , "--uploadlimit"
                                , show upload''
                                , "--numidentity"
                                , numids
                                , "--identity"
                                , show id ] | let Just upload' = readMaybe upload :: Maybe Float,
                                              let n' = fromInteger n :: Float,
                                              let upload'' = ceiling $ upload' / n',
                                              id <- ids]
                    hs <- forM args (\a -> do
                        (_, Just hout, Just herr, h) <-
                            createProcess (proc downpour a) {
                                  std_out = CreatePipe
                                , std_err = CreatePipe
                            }
                        forkIO $ hGetContents herr >>= forceGetContents
                        forkIO $ hGetContents hout >>= forceGetContents
                        return h)
                    mapM_ waitForProcess hs
                    -- Collect results
                    (_, Just hout, _, h) <-
                            createProcess (proc "cat" ["ThatFile" ++ show id | id <- ids]) { std_out = CreatePipe }
                    forkIO $ hGetContents hout >>= writeFile "ThatFile" -- FIXME add param, need to know the outfile name
                    void $ waitForProcess h
                Nothing -> return ()
        _wrongNumArgs ->
            putStrLn "Usage: \
                     \sybil_wrapper </path/to/downpour> </path/to/torrent> \
                     \</path/to/conf> </path/to/outdir> <uploadlimit> <numidentities>"
