% Note: GUISDAP-wise this is the same experiment as cp2b

COR_init

COR_pp( 0,1,1,'s',1,180,0,1)
COR_pp(180,1,1,'c',1, 6,0,1)
COR_pp(186,1,1,'b',1,30,0,1)
COR_pp(216,1,3,'s',1,80,0,2)
COR_pp(296,1,3,'c',1, 6,0,2)
COR_pp(302,1,3,'b',1,30,0,2)

COR_trilp(333,37,2,'s',36,-18,23,0:10:350,3)
COR_trilp(333+37*24,37,2,'c',36,-18,1,0:10:350,3)
COR_trilp(333+37*26,37,2,'b',36,-18,5,0:10:350,3)

COR_end
