import Test.Hspec
import Lox.Parser qualified
import Lox.Scanner qualified
import Lox.Parse qualified

main :: IO ()
main = hspec $ do
  Lox.Parser.spec
  Lox.Scanner.spec
  Lox.Parse.spec
