module Tools.Tuples where

elem3Fst :: (a,b,c) -> a
elem3Fst (a,_,_) = a
elem3Snd :: (a,b,c) -> b
elem3Snd (_,b,_) = b
elem3Trd :: (a,b,c) -> c
elem3Trd (_,_,c) = c

elem4Fst :: (a,b,c,d) -> a
elem4Fst (a,_,_,_) = a
elem4Snd :: (a,b,c,d) -> b
elem4Snd (_,b,_,_) = b
elem4Trd :: (a,b,c,d) -> c
elem4Trd (_,_,c,_) = c
elem4Fth :: (a,b,c,d) -> d
elem4Fth (_,_,_,d) = d


elemday6 :: ((Int,Int),Int) -> [((Int,Int),Int) ] -> Bool
elemday6 _ [] = False 
elemday6 ((row,col),dir) (prevStep:prevSteps) = (row == fst (fst prevStep)) && (col == snd (fst prevStep)) && (dir == snd prevStep) || elemday6 ((row,col),dir) prevSteps