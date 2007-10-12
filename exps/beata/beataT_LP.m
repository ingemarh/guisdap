N_SCAN=64;

COR_init((1301+1+1+1)*N_SCAN,63)
for vc=1:N_SCAN
    COR_fraclp(480,vc,'s',432-2*31,32,20,(1:41)*10,1)
    COR_pp(48,1,vc,'s',1,432,0,1)
    COR_pp(25563+14*431,1,vc+N_SCAN,'b',1,432,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,16,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(16,1,vc,'c',1,16,0,1)
    COR_pp(25547+14*431,1,vc+N_SCAN,'c',1,16,0,2)
end
   
COR_end
