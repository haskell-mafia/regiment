{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Regiment.Vanguard.List (
    readCursorFromList
  , writePayloadToList
  , formVanguardFromLists
  , runVanguardFromLists
  ) where

import           Control.Monad.Primitive (PrimState, PrimMonad)
import           Control.Monad.ST (runST, ST)
import           Control.Monad.Trans.Class (lift)

import qualified Data.STRef as STR

import           P

import           Regiment.Data
import           Regiment.Vanguard.Base

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)

readCursorFromList :: STR.STRef s [SortKeysWithPayload]
                   -> EitherT x (ST s) (Maybe SortKeysWithPayload)
readCursorFromList lsksp = do
  sksps <- lift $ STR.readSTRef lsksp
  case sksps of
    [] -> return Nothing
    (x : xs) -> do
      lift $ STR.writeSTRef lsksp xs
      return $ Just x

writePayloadToList :: STR.STRef s [Payload] -> Payload -> ST s ()
writePayloadToList lst p =
  STR.modifySTRef lst (\lp -> lp <> [p])

formVanguardFromLists :: (PrimMonad (ST s))
                      => [STR.STRef s [SortKeysWithPayload]]
                      -> EitherT (RegimentReadError x) (ST s) (Vanguard (PrimState (ST s)) (STR.STRef s [SortKeysWithPayload]))
formVanguardFromLists strsksps =
  formVanguard readCursorFromList strsksps

runVanguardFromLists :: [[SortKeysWithPayload]] -> Either (RegimentReadError x) [Payload]
runVanguardFromLists lsksps = runST $ runEitherT $ do
  strsksps <- lift $ mapM STR.newSTRef lsksps
  out <- lift $ STR.newSTRef []
  v <- formVanguard readCursorFromList strsksps
  runVanguard v readCursorFromList (writePayloadToList out)
  lift $ STR.readSTRef out
