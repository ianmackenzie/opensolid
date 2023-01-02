import OpenSolid
import Script (IOError, Script)
import Script qualified

tests :: Script IOError ()
tests = Script.printLine "Test suite not yet implemented"

main :: IO ()
main = Script.run tests
