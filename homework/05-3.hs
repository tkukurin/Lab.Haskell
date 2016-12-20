
data Pred = Val Bool | Or Pred Pred | And Pred Pred | Not Pred deriving (Show)

{-Or (Val first) (Val second) = Val (first || second)
And (Val first1) (Val second1) = Val (first1 && second1)-}

eval :: Pred -> Bool
eval (Val x)     = x
eval (Not x)     = not (eval x)
eval (And p1 p2) = (eval p1) && (eval p2)
eval (Or p1 p2)  = (eval p1) || (eval p2)

