inExpAr :: Either b (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a))) -> ExpAr a
inExpAr = [X , num ops]
		where
		num ops = [N,ops]
		ops = [bin,Un]
		bin (op,(a,b)) = Bin op a b

baseExpAr f g h j k l z = f + (g + (h × (j × k ) + l × z))

outExpAr :: ExpAr a -> Either () (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a)))
outExpAr = 
