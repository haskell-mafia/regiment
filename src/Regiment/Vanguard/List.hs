{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Regiment.Vanguard.List (
    readCursorList
  , writePayloadToList
  , formVanguardList
  , runVanguardList
  ) where

import           Control.Monad.Primitive (PrimState, PrimMonad)
import           Control.Monad.ST (runST, ST)
import           Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as BS
import qualified Data.STRef as ST

import           P

import           Regiment.Data
import           Regiment.Vanguard.Base

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)

readCursorList :: ST.STRef s [KeyedPayload]
               -> EitherT x (ST s) (Maybe KeyedPayload)
readCursorList lkp = do
  kps <- lift $ ST.readSTRef lkp
  case kps of
    [] -> return Nothing
    (x : xs) -> do
      lift $ ST.writeSTRef lkp xs
      return $ Just x

writePayloadToList :: ST.STRef s [BS.ByteString] -> BS.ByteString -> ST s ()
writePayloadToList lst p =
  ST.modifySTRef lst (\lp -> lp <> [p])

formVanguardList :: (PrimMonad (ST s))
                 => [ST.STRef s [KeyedPayload]]
                 -> EitherT
                    (RegimentMergeError x)
                    (ST s) (Vanguard (PrimState (ST s)) (ST.STRef s [KeyedPayload]))
formVanguardList strkps =
  formVanguard readCursorList strkps

runVanguardList :: [[KeyedPayload]] -> Either (RegimentMergeError x) [BS.ByteString]
runVanguardList kps = runST $ runEitherT $ do
  stkps <- lift $ mapM ST.newSTRef kps
  out <- lift $ ST.newSTRef []
  v <- formVanguard readCursorList stkps
  runVanguard v readCursorList (writePayloadToList out)
  lift $ ST.readSTRef out
