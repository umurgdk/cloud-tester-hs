module SafeProcess where

import System.Process
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import GHC.IO.Handle
import GHC.IO.Exception
import qualified Control.Monad.Trans.Except as E
import Control.Monad.IO.Class

import Types

run :: String -> ExceptIO String
run program = do
    let p = (shell program)
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = Inherit
            }
    
    (_, Just h, _, ph) <- liftIO $ createProcess p
    (ec, output) <- liftIO $ gatherOutput ph h
    
    case ec of
        ExitSuccess -> return $ BC.unpack output
        _ ->           E.throwE $ Err $ BC.unpack output

gatherOutput :: ProcessHandle -> Handle -> IO (ExitCode, BS.ByteString)
gatherOutput ph h = work mempty
  where
    work acc = do
        -- Read any outstanding input.
        bs <- BS.hGetNonBlocking h (64 * 1024)
        let acc' = acc <> bs
        -- Check on the process.
        s <- getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work acc'
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                last <- BS.hGetContents h
                return (ec, acc' <> last)
