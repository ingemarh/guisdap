N_SCAN=64;

COR_init((1301+1+1+1+14*31+11050+2)*N_SCAN,1599)
for vc=1:N_SCAN
  COR_fraclp(480,vc,'s',432-2*31,32,20,(1:41)*10,1)
  COR_pp(48,1,vc,'s',1,432,0,1)
  COR_pp(25531+14*431+32,1,vc+N_SCAN,'o',1,432,0,2)
  for i=0:13
    COR_fraclp(25531+i*431,vc,'s',432-2*31,32,20,10,i+3)
  end
  COR_fdalt(32061,vc+N_SCAN*2,'o',78,12,0,32,20,0,(0:800)*.4,20)
  COR_pp(32061+91*50+90*800+4*50,1,vc+N_SCAN*2,'o',1,5450,0,20)
end

for vc=1:2:N_SCAN
  COR_pp(0,1,vc,'b',1,16,0,1)
  COR_pp(25531+14*431,1,vc+N_SCAN,'b',1,16,0,2)
  COR_pp(32029,1,vc+2*N_SCAN,'b',1,16,0,20)
end
for vc=2:2:N_SCAN
  COR_pp(16,1,vc,'c',1,16,0,1)
  COR_pp(25531+14*431+16,1,vc+N_SCAN,'c',1,16,0,2)
  COR_pp(32029+16,1,vc+N_SCAN*2,'c',1,16,0,20)
end
   
COR_end
