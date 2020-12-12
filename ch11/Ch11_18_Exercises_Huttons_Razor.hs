module Ch11_18_Exercises_Huttons_Razor where

data Expr 
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add exp exp') = eval exp + eval exp'

testEval :: IO ()
testEval =
  if eval (Add (Lit 1) (Lit 9001)) == 9002
  then putStrLn "eval works!"
  else putStrLn "eval is broke!"


printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add exp exp') = printExpr exp ++ " + " ++ printExpr exp'

testPrintExpr :: IO ()
testPrintExpr =
  if printExpr (Add (Lit 1) (Lit 9001)) == "1 + 9001"
  then putStrLn "printExpr works!"
  else putStrLn "printExpr is broke!"


main :: IO ()
main = do
  testEval
  testPrintExpr