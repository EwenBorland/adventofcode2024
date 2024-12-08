module Tools.Tuples where

elem3Fst :: (a,b,c) -> a
elem3Fst (a,_,_) = a
elem3Snd :: (a,b,c) -> b
elem3Snd (_,b,_) = b
elem3Trd :: (a,b,c) -> c
elem3Trd (_,_,c) = c


elemday6 :: ((Int,Int),Int) -> [((Int,Int),Int) ] -> Bool
elemday6 _ [] = False 
elemday6 ((row,col),dir) (prevStep:prevSteps) = (row == fst (fst prevStep)) && (col == snd (fst prevStep)) && (dir == snd prevStep) || elemday6 ((row,col),dir) prevSteps