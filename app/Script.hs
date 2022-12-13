module Script (
    Script,
    IOError,
    run,
    succeed,
    fail,
    printLine,
    forEach,
) where

import Control.Exception qualified
import Data.Text.IO qualified
import OpenSolid
import System.Exit qualified
import Prelude (IOError)
import Prelude qualified

data Script x a
    = Succeed a
    | Fail x
    | Perform (IO (Script x a))

instance Functor (Script x) where
    fmap function script = script >>= (succeed . function)

instance Applicative (Script x) where
    pure = succeed
    script1 <*> script2 = script1 >>= (`fmap` script2)

instance Monad (Script x) where
    Succeed value >>= function = function value
    Fail err >>= _ = Fail err
    Perform io >>= function = Perform (fmap (>>= function) io)

perform :: IO a -> Script IOError a
perform io = Perform (Control.Exception.catch (fmap succeed io) (pure . fail))

run :: Script IOError () -> IO ()
run (Succeed ()) = System.Exit.exitSuccess
run (Fail ioError) = Prelude.ioError ioError
run (Perform io) = io >>= run

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
