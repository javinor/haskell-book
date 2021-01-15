module Ch24_03_LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

p123 s =
  parseString (string s) mempty "123"

p123' s =
  parseString (mapM char s) mempty "123"

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

  pNL "\n===== Parsing Practice ====="
  pNL "Q1) eof:"
  print $ parseString (one >> eof) mempty "12"
  print $ parseString (oneTwo >> eof) mempty "123"

  pNL "Q2) string:"
  print $ p123 "1"
  print $ p123 "12"
  print $ p123 "123"

  pNL "Q3) string using char:"
  print $ p123' "1"
  print $ p123' "12"
  print $ p123' "123"
