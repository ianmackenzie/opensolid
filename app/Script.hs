module Script (
    Script,
    IOError,
    run,
    Program,
    (>>),
    (>>=),
    succeed,
    fail,
    printLine,
    forEach,
) where

import qualified Data.Text.IO
import OpenSolid hiding ((>>))
import Script.Internal (Script, (>>), (>>=))
import qualified Script.Internal as Internal
import qualified System.Exit
import Prelude (IOError)
import qualified Prelude

type Program = Prelude.IO ()

run :: Script IOError () -> Program
run (Internal.Succeed ()) = System.Exit.exitSuccess
run (Internal.Fail ioError) = Prelude.ioError ioError
run (Internal.Perform io) = io Prelude.>>= run

succeed :: a -> Script x a
succeed = Internal.Succeed

fail :: x -> Script x a
fail = Internal.Fail

printLine :: String -> Script IOError ()
printLine string = Internal.perform (Data.Text.IO.putStrLn string)

forEach :: (a -> Script x ()) -> List a -> Script x ()
forEach _ [] = succeed ()
forEach function (first : rest) = do
    function first
    forEach function rest
