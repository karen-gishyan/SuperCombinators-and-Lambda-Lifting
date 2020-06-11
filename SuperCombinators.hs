-------------------------
-------- PART B --------- 
-------------------------

------------------------- Assignment 5

free :: Term -> [Var] 
free (Variable x) = [x] 
free (Lambda x n) = filter(/=x) (free n) 
free (Apply  n m) = merge (free n) (free m)


abstractions :: Term -> [Var] -> Term
abstractions term list 
   | length list <1 =  term 
   | otherwise = (Lambda (head list) (abstractions (term)  (tail list)))



applications :: Term -> [Term] -> Term
applications term list 
   | length list <1 =  term
   | otherwise = (Apply (applications (term) (init list)) (last list)) 
 
   
lift :: Term -> Term  
lift term= applications arg1 arg2   
  where
    arg1= abstractions (term) (free (term))    
    arg2 =  variable_convert (free (term)) 
    variable_convert []=[] 
    variable_convert (x:xs) = (Variable x): variable_convert xs



super :: Term -> Term 
super (Variable x) = (Variable x) 
super (Lambda x n) = lift (Lambda x (auxl n))
  where 
    auxl (Variable x) = super (Variable x)
    auxl (Lambda x n) = Lambda x (auxl n)
    auxl (Apply m n) = super (Apply m n) 
super (Apply m n) = Apply (super m) (super n)


------------------------- Assignment 6

data Expr =
   V Var 
  |A Expr Expr

toTerm :: Expr -> Term
toTerm (V x) = (Variable x) 
toTerm (A m n) = Apply(toTerm m) (toTerm n)  


instance Show Expr where
  show = show . toTerm

type Inst = (Var, [Var],Expr) 

data Prog = Prog [Inst]


instance Show Prog where
  show (Prog ls) = unlines (map showInst ks)
    where
      ks = map showParts ls
      n  = maximum (map (length . fst) ks)
      showParts (x,xs,e) = (x ++ " " ++ unwords xs , show e)
      showInst (s,t) = take n (s ++ repeat ' ') ++ " = " ++ t


names = ['$':show i | i <- [1..] ]

-------------------------

stripAbs :: Term -> ([Var],Term)
stripAbs (Variable x) = ([],Variable x) 
stripAbs (Lambda x z) = (([x]++ (fst (stripAbs z))), (auxl z))
  where  
    auxl (Variable x) = Variable x  
    auxl (Lambda x n) = auxl n 
    auxl (Apply m n) =  Apply (snd (stripAbs m)) n 
stripAbs (Apply m n) =  ([], Apply m n )

 
takeAbs :: Term -> [Term] 
takeAbs  (Variable x) = []
takeAbs  (Lambda x n) = [Lambda x n] ++ function (n) 
 where 
  function (Variable x) = takeAbs (Variable x) 
  function (Lambda x n) = function n 
  function (Apply m n) =  []  
takeAbs  (Apply m n) = takeAbs(m) ++ takeAbs(n)


toExpr :: [Var] -> Term -> Expr
toExpr (list) (Variable z) = V z
toExpr (list) (Lambda y n) = V (head (list))
toExpr (list) (Apply m n) = A (toExpr (list) m) (toExpr second_list n)
 where
  second_list= drop (length (takeAbs m)) list



toInst :: [Var] -> (Var,Term) -> (Inst,[(Var,Term)],[Var])
toInst fresh_list (instruct_var, term) = ((instruct_var, fst (stripAbs (term)), toExpr (fresh_list) (snd (stripAbs term))), f, (drop (length (takeAbs term)) fresh_list))
 where
  f = myfunction (fresh_list) (takeAbs (snd (stripAbs term)))
  myfunction:: [Var] -> [Term] -> [(Var,Term)]
  myfunction _ [] = []   
  myfunction (x:xs) (y:ys) = (x,y) : (myfunction (xs) ys) 



prog :: Term -> Prog
prog term = Prog (aux names [("$main", super term)])
  where
    aux :: [Var] -> [(Var,Term)] -> [Inst]
    aux _ [] = []      
    aux (y:ys) (x:xs) = fst1 (toInst (y:ys) x) : (aux second_list (xs++ (snd2 (toInst (y:ys) x))))  
     where 
      second_list= drop (length (takeAbs (snd x))) (y:ys)
      fst1 :: (a, b, c) -> a
      fst1 (x, _, _) = x
      snd2 :: (a, b, c) -> b
      snd2 (_, y, _) = y


example2 = Apply (Variable "S") (Apply (Apply example (numeral 0)) (Variable "0"))
example3 = Apply (Apply add (numeral 1)) (Apply (Apply mul (numeral 2)) (numeral 3))
example4 = Apply (Apply example3 (Variable "S")) (Variable "0")



------------------------- Assignment 7

sub :: [(Var,Expr)] -> Expr -> Expr

sub [] expr = expr
sub ((var, expr) :xs) (V x) 
  | var==x = expr
  | otherwise= sub (xs) (V x)
sub ((var, n):xs) (A m t) = A (sub ((var, n):xs) m) (sub ((var, n):xs) t)
