module Types where

import qualified Control.Monad.Trans.Except as E

newtype Err = Err String
type ExceptIO = E.ExceptT Err IO

runExceptIOT = E.runExceptT

