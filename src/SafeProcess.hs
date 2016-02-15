module SafeProcess where

import           System.Process

import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           GHC.IO.Exception
import           GHC.IO.Handle

import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.Except as E

import           Types

run :: String -> ExceptIO T.Text
run program = do
    let p = (shell program)
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = Inherit
            }

    (_, Just h, _, ph) <- liftIO $ createProcess p
    (ec, output) <- liftIO $ gatherOutput ph h

    case ec of
        ExitSuccess -> return output
        _ ->           throwE output

gatherOutput :: ProcessHandle -> Handle -> IO (ExitCode, T.Text)
gatherOutput ph h = work mempty
  where
    work acc = do
        -- Read any outstanding input.
        bs <- TIO.hGetChunk h
        let acc' = acc <> bs
        -- Check on the process.
        s <- getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work acc'
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                last <- TIO.hGetContents h
                return (ec, acc' <> last)
