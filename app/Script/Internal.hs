module Script.Internal (
    Script (..),
    (>>),
    (>>=),
    perform,
) where

import qualified Control.Exception
import qualified OpenSolid
import Prelude (IO, IOError)
import qualified Prelude

data Script x a
    = Succeed a
    | Fail x
    | Perform (IO (Script x a))

(>>) :: Script x () -> Script x a -> Script x a
(>>) script1 script2 =
    script1 >>= (\() -> script2)

(>>=) :: Script x a -> (a -> Script x b) -> Script x b
(>>=) script function =
    case script of
        Succeed value ->
            function value
        Fail error ->
            Fail error
        Perform io ->
            Perform (Prelude.fmap (>>= function) io)

perform :: IO a -> Script IOError a
perform io =
    Perform (Control.Exception.catch (Prelude.fmap Succeed io) (Fail OpenSolid.>> Prelude.return))
