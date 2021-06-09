

inExpAr :: Either b (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a))) -> ExpAr a
inExpAr = [X, num ops]
		where
		num ops = [N,ops]
		ops = [bin,Un]
		bin (op,(a,b)) = Bin op a b

baseExpAr f g h j k l z = f + (g + (h × (j × k) + l × z))


-- Ponto 1.

outExpAr X = i1 ()
outExpAr (N y) = i1 (i2 y)
outExpAr (bin (op, (a,b))) = i1 (i2 (i2 (op, (a,b))))
outExpAr (un (op,a)) = i2 (i2 (i2 (op,a)))

recExpAr f g h j k l z = baseExpAr f g h j k l z


-- Ponto 2.

eval_exp a = (| g |)
<=>
eval_exp a . in  = g . F eval_exp a
<=>
eval_exp a . [X, num ops] = g . (f + (g + (h × (j × k) + l × z)))
<=>
eval_exp a . [X, num ops] = [g1, g2] . (f + (g + (h × (j × k) + l × z)))
<=>
| eval_exp a . X a = g1 . f
| eval_exp a . num ops = [g3, g4] . (g + (h × (j × k) + l × z))
<=>
| eval_exp a . X = g1 . f
| | eval_exp a . N = g3 . g
| | eval_exp a . ops = g4 . (h × (j × k) + l × z)
<=>
| eval_exp a . X = g1 . f
| | eval_exp a . N = g3 . g
| | | eval_exp a . bin = g5 . (h x (j x k))
| | | eval_exp a . Un = g6 . (l x z)
<=>
| eval_exp a X = g1 ( f () )
| | eval_exp a (N y) = g3 (g y)
| | | eval_exp a (bin (op, (a,b))) = g5 ( h op, (j a, k b) )
| | | eval_exp a (un (op,a)) = g6 ( l op, z a )
<=>
| cataExpAr (g_eval_exp a X) = g1 ( f () )
| | cataExpAr (g_eval_exp a (N y)) = g3 (g y)
| | | cataExpAr (g_eval_exp a (bin (op, (a,b)))) = g5 ( h op, (j a, k b) )
| | | cataExpAr (g_eval_exp a (un (op,a))) = g6 ( l op, z a )



avg [a] = a
avg (a:x) = ( a + k (avg x) / (k + 1) ) where k = length x 
<=>
avg . singl a = id a
avg . cons (a,x) = (a + k (avg x)) / (k + 1) where k = length x 
<=>
avg . singl = id
avg . cons (a,x) = (a + length (avg x)) * (1/(succ (length x))
<=>
avg . nil = 0
avg . cons (a,x) = (add (id x (length . avg)) (a,x)) * (1/((succ . length) x))
<=>
avg . nil = 0
avg . cons (a,x) = (add (id x (length . (avg))) (a,x)) / (succ . length) x
<=>
avg . 


out [x] = i1 x 
out (x:t) = i2 (x,t)

rec 
---------------------------------------------------------//---------------------------------------------------------


          out
X ------------------> A + (B + C)

X a = i1
X b = (i2 (i1 b))
X c = (i2 (i2 c))


---------------------------------------------------------//---------------------------------------------------------


> ghci cp2021t.lhs
> :t inExpAr