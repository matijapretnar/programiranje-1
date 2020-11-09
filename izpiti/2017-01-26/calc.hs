-- 1: arith expr : eval, Num, collected form
data Expr = Integer Integer | Plus Expr Expr | Times Expr Expr | Negate Expr deriving Eq


-- λ> eval (Times (Plus (Integer 3) (Integer 2)) (Integer 2))
-- 10
-- λ> eval (Times (Times (Integer 3) (Integer 2)) (Plus (Integer 4) (Integer 3)))
-- 42

eval :: Expr -> Integer
eval (Integer x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)
eval (Negate x) = - (eval x)

instance Num Expr where
  x + y = Plus x y
  x * y = Times x y
  fromInteger x = Integer x
  abs x = fromInteger (abs (eval x))
  signum x = fromInteger (signum (eval x))
  negate x = Negate x


-- this is a little hairy
data ExprPlusInt = LPlus [ExprTimesInt] | LitPlus Integer
data ExprTimesInt = LTimes [ExprTimesInt] | LitTimes Integer
data ExprLst = LstPlus ExprPlusInt | LstTimes ExprTimesInt

compress :: Expr -> ExprLst
compress (Integer x) = LstPlus (LitPlus x)
