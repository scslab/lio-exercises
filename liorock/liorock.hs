{-# LANGUAGE Safe #-}
module Main where

import Control.Monad

import LIO
import LIO.Concurrent
import LIO.DCLabel

import NetLib

port :: PortID
port = PortNumber 1617

runGame :: Handle -> LIO DCLabel ()
runGame h = do
  hPutStrLn h "hello world"
  return ()

main :: IO ()
main = withSocketsDo $ do
  evalDC $ do
    (sock, refereePriv) <- listenOn port
    logP refereePriv $ "Listening on " ++ show port
    setClearance (refereePriv %% True)
    forever $ do
      (h1, p1) <- acceptP refereePriv sock
      (h2, p2) <- acceptP refereePriv sock

      forkLIO $ (runGame h1
                  `finally` hCloseP refereePriv h1)
                  `catch` (printErr refereePriv)

      forkLIO $ (runGame h2
                  `finally` hCloseP refereePriv h2)
                  `catch` (printErr refereePriv)

printErr :: DCPriv -> SomeException -> DC ()
printErr p e = logP p $ show e

