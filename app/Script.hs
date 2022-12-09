module Script (
    Script,
    IOError,
    run,
    (>>),
    (>>=),
    succeed,
    fail,
    printLine,
    forEach,
) where

import Control.Exception qualified
import Data.Text.IO qualified
import OpenSolid hiding ((>>))
import System.Exit qualified
import Prelude (IOError)
import Prelude qualified

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

run :: Script IOError () -> IO ()
run (Succeed ()) = System.Exit.exitSuccess
run (Fail ioError) = Prelude.ioError ioError
run (Perform io) = io Prelude.>>= run

succeed :: a -> Script x a
succeed = Succeed

fail :: x -> Script x a
fail = Fail

printLine :: Text -> Script IOError ()
printLine text = perform (Data.Text.IO.putStrLn text)

forEach :: (a -> Script x ()) -> List a -> Script x ()
forEach _ [] = succeed ()
forEach function (first : rest) = do
    function first
    forEach function rest
