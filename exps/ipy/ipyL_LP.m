N_SCAN=64;

COR_init((1179+732+1+1+3/2)*N_SCAN,63)
for vc=1:N_SCAN
    COR_fraclp(182,vc,'s',152-2*29,30,30,(1:41)*15,1)
    COR_fdalt(8153,vc,'s',0,0,28,30,30,94,(0:30)*15,1)
    COR_pp(30,1,vc,'s',1,152,0,1)
    COR_pp(9911,1,vc+N_SCAN,'b',1,152,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
    COR_pp(9901,1,vc+N_SCAN,'c',1,10,0,2)
end
   
COR_end
