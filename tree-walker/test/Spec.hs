import Test.Hspec
import Lox.Parser qualified
import Lox.Scanner qualified

main :: IO ()
main = hspec $ do
  Lox.Parser.spec
  Lox.Scanner.spec
