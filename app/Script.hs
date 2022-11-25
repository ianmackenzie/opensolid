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
run script =
    case script of
        Internal.Succeed () ->
            System.Exit.exitSuccess
        Internal.Fail ioError ->
            Prelude.ioError ioError
        Internal.Perform io ->
            io Prelude.>>= run

succeed :: a -> Script x a
succeed =
    Internal.Succeed

fail :: x -> Script x a
fail =
    Internal.Fail

printLine :: String -> Script IOError ()
printLine string =
    Internal.perform (Data.Text.IO.putStrLn string)

forEach :: (a -> Script x ()) -> List a -> Script x ()
forEach function values =
    case values of
        [] -> Script.succeed ()
        first : rest -> do
            function first
            forEach function rest
