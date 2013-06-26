{-# LANGUAGE Trustworthy #-}

module NetLib where
import safe qualified System.IO as IO
import safe LIO
import safe LIO.DCLabel
import LIO.TCB
import LIO.TCB.LObj

type Handle = LObj DCLabel IO.Handle

hPutStrLnP :: DCPriv -> Handle -> String -> LIO DCLabel ()
hPutStrLnP = blessPTCB "hPutStrLnP" IO.hPutStrLn

hPutStrLn :: Handle -> String -> LIO DCLabel ()
hPutStrLn = blessTCB "hPutStrLn" IO.hPutStrLn

stdout :: Handle
stdout = LObjTCB (True %% True) IO.stdout

myPriv :: DCPriv
myPriv = PrivTCB $ toCNF True

test = evalDC $ hPutStrLnP myPriv stdout "Hello world"
