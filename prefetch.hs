{-# OPTIONS -Wall -O #-}
{-# LANGUAGE LambdaCase #-}
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import qualified Data.ByteString as BS
import           System.IO (Handle, stdin, stdout)

handleToChan :: Handle -> Chan (Maybe BS.ByteString) -> IO ()
handleToChan handle chan =
    do
        buf <- BS.hGetSome handle (128*1024)
        if BS.null buf
            then writeChan chan Nothing
            else writeChan chan (Just buf) >> handleToChan handle chan

chanToHandle :: Chan (Maybe BS.ByteString) -> Handle -> IO ()
chanToHandle chan handle =
    readChan chan >>=
    \case
    Nothing -> pure ()
    Just buf -> BS.hPutStr handle buf >> chanToHandle chan handle

main :: IO ()
main = do
    bufs <- newChan
    _tid <- forkIO (stdin `handleToChan` bufs)
    bufs `chanToHandle` stdout
