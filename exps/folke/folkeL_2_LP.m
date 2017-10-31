N_SCAN=32*5;

COR_init((5*(1+558)+3*335+2+2+1+2)*32,35)
for vc=sort([1:3:N_SCAN 2:3:N_SCAN])
  COR_pp(245,1,vc,'s',1,229,0,1)
  COR_fraclp(474,vc,'s',229-2*15,16,100,(1:31)*25,1)
  COR_fdalt(11508,vc,'s',0,0,14,16,100,277-15*4,(0:32)*25,1)
end
for vc=1:3:N_SCAN
  COR_pp(225,1,vc,'b',1,10,0,1)
end
for vc=2:3:N_SCAN
  COR_pp(225+10,1,vc,'c',1,10,0,1)
end
for vc=2:6:N_SCAN
  COR_pp(0,1,vc,'b',1,117,0,1)
end
for vc=3:3:N_SCAN
  COR_pp(12016,1,vc,'b',1,229,0,3)
  COR_pp(12016+229,1,vc,'c',1,10,0,3)
  COR_pp(12471,1,vc,'s',1,117,0,3)
  COR_fraclp(12588,vc,'s',117-2*15,16,50,(1:31)*25,3)
  COR_fdalt(17133,vc,'s',0,0,14,16,100,117-15*2,(0:15)*25,3)
end
COR_end
