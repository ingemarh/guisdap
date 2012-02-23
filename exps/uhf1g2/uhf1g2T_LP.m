COR_init

for vc=5:6
  COR_pp( 0,1,vc,'s',3,63,0,1)
end
for vc=7:8
  COR_pp(810,1,vc,'s',3,63,0,1)
end

for vc=9:10
  COR_uprog(73,1,vc,'s',3,77,0:30:300,0,1)
  COR_pp(63,1,vc,'b',27, 8,0,1)
  COR_pp(71,1,vc,'c',27, 2,0,1)
end
for vc=11:12
  COR_uprog(883,1,vc,'s',3,77,0:30:300,0,1)
  COR_pp(873,1,vc,'b',27, 8,0,1)
  COR_pp(881,1,vc,'c',27, 2,0,1)
end

for vc=3:4
  COR_pp(1620,1,vc,'b',5,10,0,2)
  COR_pp(1630,1,vc,'c',5,2,0,2)
  COR_pp(1632,1,vc,'s',5,30,0,2)
end

for vc=1:2
  COR_lp(1662,24,vc,'b',15,0, 5,0:14:322,4)
  COR_lp(1782,24,vc,'c',15,0, 1,0:14:322,4)
  COR_lp(1806,24,vc,'s',15,0,10,0:14:322,4)
end

COR_end
