module Script.Internal (
    Script (..),
    (>>),
    (>>=),
    perform,
) where

import qualified Control.Exception
import OpenSolid hiding ((>>))
import Prelude (IOError)

data Script x a
    = Succeed a
    | Fail x
    | Perform (IO (Script x a))

(>>) :: Script x () -> Script x a -> Script x a
script1 >> script2 = script1 >>= (\() -> script2)

(>>=) :: Script x a -> (a -> Script x b) -> Script x b
Succeed value >>= function = function value
Fail err >>= _ = Fail err
Perform io >>= function = Perform (fmap (>>= function) io)

perform :: IO a -> Script IOError a
perform io = Perform (Control.Exception.catch (fmap Succeed io) (pure << Fail))
