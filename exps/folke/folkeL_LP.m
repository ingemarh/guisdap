N_SCAN=32*5;

COR_init((5*(1+558)+3*335+2+2+1+2)*32,35)
for vc=[1:5:N_SCAN 3:5:N_SCAN]
  COR_pp(279,1,vc,'s',1,277,0,1)
  COR_fraclp(556,vc,'s',277-3*15,16,60,(1:35)*20,1)
end
for vc=[2:5:N_SCAN 4:5:N_SCAN]
  COR_pp(15565,1,vc,'s',1,277,0,2)
  COR_fraclp(15842,vc,'s',277-3*15,16,60,(1:35)*20,2)
  COR_fdalt(30572,vc,'s',0,0,14,16,60,277-15*3,(0:24)*20,2)
end
for vc=1:5:N_SCAN
  COR_pp(259,1,vc,'b',1,10,0,1)
  COR_pp(15545,1,vc+1,'b',1,10,0,2)
end
for vc=3:5:N_SCAN
  COR_pp(259+10,1,vc,'c',1,10,0,1)
  COR_pp(15545+10,1,vc+1,'c',1,10,0,2)
end
for vc=3:10:N_SCAN
  COR_pp(0,1,vc,'b',1,131,0,1)
  COR_pp(15286,1,vc+1,'b',1,131,0,2)
end
for vc=5:5:N_SCAN
  COR_pp(30953,1,vc,'b',1,277,0,3)
  COR_pp(30953+277,1,vc,'c',1,10,0,3)
  COR_pp(31368,1,vc,'s',1,131,0,3)
  COR_fraclp(31499,vc,'s',131-3*15,16,60,(1:35)*20,3)
  COR_fdalt(37907,vc,'s',0,0,14,16,60,131-15*3,(0:24)*20,3)
end
COR_end
