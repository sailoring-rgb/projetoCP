

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

eval_exp = (| g |)
<=>
eval_exp . in = g . F eval_exp
<=>
eval_exp . [X, num ops] = g . (f + (g + (h × (j × k) + l × z)))
<=>
eval_exp . [X, num ops] = [g1, g2] . (f + (g + (h × (j × k) + l × z)))
<=>
| eval_exp . X = g1 . f
| eval_exp . num ops = [g3, g4] . (g + (h × (j × k) + l × z))
<=>
| eval_exp . X = g1 . f
| | eval_exp . N = g3 . g
| | eval_exp . ops = g4 . (h × (j × k) + l × z)
<=>
| eval_exp . X = g1 . f
| | eval_exp . N = g3 . g
| | | eval_exp . bin = g5 . (h x (j x k))
| | | eval_exp . Un = g6 . (l x z)
<=>
| eval_exp X = g1 ( f () )
| | eval_exp (N y) = g3 (g y)
| | | eval_exp (bin (op, (a,b))) = g5 ( h op, (j a, k b) )
| | | eval_exp (un (op,a)) = g6 ( l op, z a )
<=>
| cataExpAr (g_eval_exp X) = g1 ( f () )
| | cataExpAr (g_eval_exp (N y)) = g3 (g y)
| | | cataExpAr (g_eval_exp (bin (op, (a,b)))) = g5 ( h op, (j a, k b) )
| | | cataExpAr (g_eval_exp (un (op,a))) = g6 ( l op, z a )
<=>



---------------------------------------------------------//---------------------------------------------------------


          out
X ------------------> A + (B + C)

X a = i1
X b = (i2 (i1 b))
X c = (i2 (i2 c))


---------------------------------------------------------//---------------------------------------------------------


> ghci cp2021t.lhs
> :t inExpAr