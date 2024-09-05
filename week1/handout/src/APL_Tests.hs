module APL_Tests (tests) where

import APL ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ()
import APL (Exp (..), Val (..), eval)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
{-      testCase "Convert" $
        eval (CstInt 5)
        @?=  
        (ValInt 5),

      testCase "Add" $
        eval (Add (CstInt 5) (CstInt 8)) 
        @?= 
        ValInt 13,

      testCase "Sub" $
        eval (Sub (CstInt 5) (CstInt 8)) 
        @?= 
        ValInt (-3),

      testCase "Mul" $
        eval (Mul (CstInt 5) (CstInt 8)) 
        @?=
        ValInt 40,

      testCase "Div" $
        eval (Div (CstInt 8) (CstInt 8))
        @?=
        ValInt 1,

      {-testCase "DivZero" $ 
        eval (Div (CstInt 5) (CstInt 0)) 
        @?=
        ValInt 2,
        --always fails--
        -}

      testCase "Pow" $
        eval (Pow (CstInt 2) (CstInt 5)) 
        @?= 
        ValInt 32,
-}
      testCase "Combo" $
        eval (Add (Sub (CstInt 9) (CstInt 5)) (Mul (Pow (CstInt 2) (CstInt 3)) (Div (CstInt 8) (CstInt 4))) )
        @?= 
        Right(ValInt 20),


      testCase "Convert" $
        eval (CstInt 5) 
        @?= 
        Right (ValInt 5),

      testCase "Add" $
        eval (Add (CstInt 5) (CstInt 8)) 
        @?= 
        Right(ValInt 13),

      testCase "Div0" $
        eval (Div (CstInt 5) (CstInt 0)) 
        @?= 
        Left "Division by 0",

      testCase "Div02" $
        eval (Div (CstInt 2) (Sub (CstInt 2) (CstInt 2) )) 
        @?= 
        Left "Division by 0",

      testCase "Div" $
        eval (Div (CstInt 5) (CstInt 2)) 
        @?= 
        Right(ValInt 2),
  
      testCase "PowNegativeExponent" $
        eval (Pow (CstInt 5) (CstInt (-4))) 
        @?= 
        Left "Negative exponents not allowed",

      testCase "PowNegativeBase" $
        eval (Pow (CstInt (-4)) (CstInt 2)) 
        @?= 
        Right(ValInt 16),

      testCase "Pow" $
        eval (Pow (CstInt 5) (CstInt 2)) 
        @?= 
        Right(ValInt 25),
  
      testCase "ConvertBool" $
        eval (CstBool True)
        @?= 
        Right(ValBool True),

      testCase "EqlTrueTrue" $
        eval (Eql (CstBool True) (CstBool True))
        @?= 
        Right(ValBool True),

      testCase "EqlFalseTrue" $
        eval (Eql (CstBool True) (CstBool False))
        @?= 
        Right(ValBool False),

      testCase "Eql5_4" $
        eval (Eql (CstInt 5) (CstInt 4))
        @?= 
        Right(ValBool False),

      testCase "Eql5_5" $
        eval (Eql (CstInt 5) (CstInt 5))
        @?= 
        Right(ValBool True),

      testCase "EqlErrs" $
        eval (Eql (Div (CstInt 5) (CstInt 0)) (Pow (CstInt 5) (CstInt (-2))))
        @?= 
        Left "Cant compare with error",

      testCase "EqlDifferentTypes" $
        eval (Eql (CstInt 1) (CstBool True))
        @?= 
        Left "Comparison of different types not allowed",

      testCase "If1" $
        eval (If (CstBool True) (CstInt 4) (CstInt 3))
        @?= 
        Right (ValInt 4),

      testCase "If2" $
        eval (If (CstBool False) (CstInt 4) (CstInt 3))
        @?= 
        Right (ValInt 3)
    ]