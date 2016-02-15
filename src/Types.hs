module Types where

import           Data.Text                  as T

import qualified Control.Monad.Trans.Except as E

newtype Err = Err T.Text
type ExceptIO = E.ExceptT Err IO

runExceptIOT = E.runExceptT

throwE :: T.Text -> ExceptIO a
throwE = E.throwE . Err
