{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive 
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \w -> ((RealWorld (tail  (stdIn w)) (stdOut w) (exitCode w)), head (stdIn w)) 

putNat :: Nat -> IO ()
putNat n = State $ \w -> ((RealWorld (Cons n (stdIn w)) (stdOut w) (exitCode w)), ())

setExitCode :: Nat -> IO ()
setExitCode n = State $ \w -> ((RealWorld (stdIn w) (stdOut w) (n)), ())
