{-# LANGUAGE Safe #-}
module Main where

import Control.Monad

import LIO
import LIO.Concurrent
import LIO.DCLabel

import NetLib

port :: PortID
port = PortNumber 1617

type Move = String

runGame :: (Move -> DC Bool)
        -> DCPriv
        -> Handle
        -> LIO DCLabel ()
runGame movefn p h = do
  setLabelP p (labelOf h)
  move <- hGetLine h
  result <- movefn move
  hPutStrLnP p h $ show result
  return ()

movefn :: DCPriv -> LMVar DCLabel Move -> LMVar DCLabel Move -> Move -> DC Bool
movefn p us them ourMove = do
  putLMVarP p us ourMove
  theirMove <- takeLMVarP p them

  return $ ourMove == theirMove

main :: IO ()
main = withSocketsDo $ do
  evalDC $ do
    (sock, refereePriv) <- listenOn port
    logP refereePriv $ "Listening on " ++ show port
    setClearance (refereePriv %% True)
    let llog = logP refereePriv
    forever $ do
      (h1, p1) <- acceptP refereePriv sock
      (h2, p2) <- acceptP refereePriv sock

      lmvar1 <- newEmptyLMVarP refereePriv (refereePriv %% refereePriv)
      lmvar2 <- newEmptyLMVarP refereePriv (refereePriv %% refereePriv)

      let priv1 = delegate refereePriv (refereePriv \/ p1)
      let priv2 = delegate refereePriv (refereePriv \/ p2)

      forkLIO $ (runGame (movefn refereePriv lmvar1 lmvar2) priv1 h1
                  `finally` hCloseP refereePriv h1)
                  `catch` (printErr refereePriv)

      forkLIO $ (runGame (movefn refereePriv lmvar2 lmvar1) priv2 h2
                  `finally` hCloseP refereePriv h2)
                  `catch` (printErr refereePriv)


printErr :: DCPriv -> SomeException -> DC ()
printErr p e = logP p $ show e

