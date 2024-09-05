module APL
  (
    Exp(..),
    Val(..),
    eval,
    eval
  )
where

data Exp
  = CstInt Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | CstBool Bool
  | Eql Exp Exp
  | If Exp Exp Exp

  deriving (Eq, Show)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String
{-
eval :: Exp -> Val
eval (CstInt x) = (ValInt x)
eval (Add x y) = ValInt(xVal + yVal)
  where 
    ValInt xVal = eval x
    ValInt yVal = eval y

eval (Sub x y) = ValInt(xVal - yVal)
  where
      ValInt xVal = eval x
      ValInt yVal = eval y

eval (Mul x y) = ValInt (xVal * yVal)
  where
    ValInt xVal = eval x
    ValInt yVal = eval y

eval (Div x y) = 
  if yVal == 0 then error "cant divide by 0"
  else ValInt (xVal `div` yVal)
  where
    ValInt xVal = eval x
    ValInt yVal = eval y

eval (Pow x y) = 
  if yVal < 0 then error "negative exponents not supported for ints"
  else ValInt (xVal ^ yVal)
  where
    ValInt xVal = eval x
    ValInt yVal = eval y
-}

eval :: Exp -> Either Error Val
eval (CstInt x) = Right $ ValInt x

eval (Add x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt xVal), Right (ValInt yVal)) -> Right $ ValInt $ xVal + yVal

eval (Sub x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt xVal), Right (ValInt yVal)) -> Right $ ValInt $ xVal - yVal

eval (Mul x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt xVal), Right (ValInt yVal)) -> Right $ ValInt $ xVal * yVal

eval (Div x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (_, Right (ValInt 0)) -> Left $ "Division by 0"
    (Right (ValInt xVal), Right (ValInt yVal)) -> Right $ ValInt $ xVal `div` yVal

eval (Pow x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt xVal), Right (ValInt yVal)) -> 
      if yVal < 0 then Left "Negative exponents not allowed"
      else Right $ ValInt $ xVal ^ yVal

eval (CstBool x) = Right $ ValBool x

eval (Eql x y) = 
  case (eval x, eval y) of
    (Left err, _) -> Left "Cant compare with error"
    (_, Left err) -> Left "Cant compare with error"
    (Right (ValBool v), Right (ValInt r)) -> Left "Comparison of different types not allowed"
    (Right (ValInt v), Right (ValBool r)) -> Left "Comparison of different types not allowed"
    (e1, e2) -> if e1 == e2 then Right $ ValBool $ True else Right $ ValBool $ False
    
eval (If cond e1 e2) =
  case eval cond of
    Left err -> Left err
    Right (ValBool True) -> eval e1
    Right (ValBool False) -> eval e2
    Right _ -> Left "not a bool"
