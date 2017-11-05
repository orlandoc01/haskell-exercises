module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof
one' = one >> stop

oneTwo = char '1' >> char '2' >> eof
oneTwo' = oneTwo >> stop

testParse :: Parser () -> IO ()
testParse p = print $ parseString p mempty "123"

customString :: CharParsing m => String -> m String
customString = traverse char

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one: "
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

