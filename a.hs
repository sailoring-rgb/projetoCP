

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
outExpAr (bin (op, (a,b))) = i1.i2.i2 (op, (a,b))
outExpAr (un (op,a)) = i2.i2.i2 (op,a)

recExpAr f g h j k l z = baseExpAr f g h j k l z


-- Ponto 2.


---------------------------------------------------------//---------------------------------------------------------


          out
X ------------------> A + (B + C)

X a = i1
X b = (i2 (i1 b))
X c = (i2 (i2 c))


---------------------------------------------------------//---------------------------------------------------------


> ghci cp2021t.lhs
> :t inExpAr