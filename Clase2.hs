--1)
estanRelacionados :: (Num t, Ord t) => t -> t -> Bool
estanRelacionados x y =  (x<=3 && y<=3) || (x<=7 && x>3) && (y<=7 && y>3) || (x>7 && y>7)

--2)
prodInt :: (Num t) => (t, t) -> (t,t) -> t 
prodInt (v1, v2) (w1, w2) = (v1*w1) + (v2*w2) 

--3)
todoMenor :: (Ord t) => (t, t) -> (t,t) -> Bool
todoMenor (v1,v2) (w1,w2) = fst(v1,v2) < snd(w1,w2) && snd(v1,v2) < snd(w1,w2) 

--4)
distanciaPuntos :: (Floating t, Num t) => (t, t) -> (t,t) -> t
distanciaPuntos (p1,p2) (q1,q2) = sqrt(((p1-q1)**2) - ((p2-q2)**2))

--5)
sumaTerna :: (Num t) => (t, t, t) -> t
sumaTerna (t1,t2,t3) = t1+ t2 +t3

--6)
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (t1,t2,t3) | even t1 = 1
                          | even t2 = 2
                          | even t3 = 3
                          | otherwise = 4

--7)
crearPar :: a -> b -> (a, b)
crearPar n1 n2 = (n1,n2)

--8)
invertir :: (a, b) -> (b, a)
invertir (n1,n2) = (n2,n1)
