import Test.Hspec
import Lox.Parser qualified

main :: IO ()
main = hspec $ do
  Lox.Parser.spec
