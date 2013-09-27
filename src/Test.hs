module Main where
import Text.ParserCombinators.Parsec
import Monad
import Test.HUnit
import System
import VData
import V

e (Right a) = a
e (Left a) = VStr_ (show a)
ea (Right a) = a
ea (Left a) = [(VStr_ (show a))]
x (Right a) = a
x (Left a) = (show a)

tstparse name fn i o = TestCase $ assertEqual name o $ e $ parse fn "test.v" i
tstparsea name fn i o = TestCase $ assertEqual name o $ ea $ parse fn "test.v" i

tX :: Parser String
tX = do x <- many1 digit
        return x


lX = do skipMany space
        x <- tX `sepBy` space
        skipMany space
        return (concat x)


plst = [
       TestCase $ assertEqual "X1" "5" $ x (parse tX "test.v" "5"),
       TestCase $ assertEqual "X2" "68" $ x (parse lX "test.v" "6 8"),
       TestCase $ assertEqual "X3" "68" $ x (parse lX "test.v" " 6 8"),
       TestCase $ assertEqual "X4" "68" $ x (parse lX "test.v" "6 8 ")
       ]

tlst = [
       tstparse "symbol.1" parseSymbol "$" (VSym_ "$"),
       tstparse "symbol.2" parseSymbol "a" (VSym_ "a"),
       tstparse "symbol.3" parseSymbol "aa" (VSym_ "aa"),
       tstparse "int.1" parseNumber "5" (VInt_ 5),
       tstparse "int.2" parseNumber "1112" (VInt_ 1112),
       tstparse "string.1" parseString "\"a\"" (VStr_ "a"),
       tstparse "string.2" parseString "\"223 \\t  \\   \\\" \\\\ aa\"" (VStr_ "223 \t  <^ >  \" \\ aa"),
       tstparsea "expr.1" parseExprList "5 4 +" [VInt_ 5, VInt_ 4,VSym_ "+"],
       tstparsea "expr.2" parseExprList " 5 4 +" [VInt_ 5, VInt_ 4,VSym_ "+"],
       tstparsea "expr.3" parseExprList "5 4 + " [VInt_ 5, VInt_ 4,VSym_ "+"]
       ]
dlst = [
       TestCase $ do y <- v "[a 1]. [b a]. [a 100]. b b + " ; assertEqual "v.lexical.1" "[2]" y,
       TestCase $ do y <- v "[c 11]. [a [b 10]. [c 100] . b c +]. a";  assertEqual "v.lexical.2" "[110]" y,
       TestCase $ do y <- v "[c 11]. [a [b 10]. b c +]. a"; assertEqual "v.lexical.3" "[21]" y,
       TestCase $ do y <- v "[a [b 10]. [c 100] . b c +]. a"; assertEqual "v.lexical.4" "[110]" y,
       TestCase $ do y <- v "2 dup"; assertEqual "v.dup.1" "[2,2]" y,
       TestCase $ do y <- v "2 dup +"; assertEqual "v.dup.2" "[4]" y,
       TestCase $ do y <- v "3 2 1 dup +"; assertEqual "v.dup.3" "[2,2,3]" y,
       TestCase $ do y <- v "3 2 1 swap"; assertEqual "v.swap.1" "[2,1,3]" y,
       TestCase $ do y <- v "0 1 2 3 4 [5 6 7] i"; assertEqual "v.i.1" "[7,6,5,4,3,2,1,0]" y,
       TestCase $ do y <- v "1 2 3 [2 3 4 + +] i"; assertEqual "v.i.2" "[9,3,2,1]" y,
       TestCase $ do y <- v "[a [b 1]. [c 2]. [b c] i]. [b 100]. [c 200]. a"; assertEqual "v.i.3" "[2,1]" y,
-- the quote on its own does not capture the environment.
       TestCase $ do y <- v "[b 1]. [c 2]. [b c] [b 100]. [c 200]. i"; assertEqual "v.i.4" "[200,100]" y,
       TestCase $ do y <- v "" ; assertEqual "v.[]" "[]" y
       ]


showRes c@(Counts cases tried err fail) = "\n-> " ++ show c

mytest = runTestTT $ TestList (tlst ++ dlst)

main = do c <- Main.mytest
          System.exitWith $ codeGet (errors c) (failures c)


codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess

