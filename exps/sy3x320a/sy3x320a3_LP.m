COR_init
nocal=1;
N_SCAN=8;

COR_init((128+765+128+128)*N_SCAN/2,129)
for vc=1:2:N_SCAN
    COR_uprog(0,1,vc,'s',1,4000,(0:127)*2.5,35,85)
    COR_fraclp(503872,vc,'s',4000-128*2,3,320,(1:383)*2.5,85,35)
    COR_uprog(2445952,1,vc,'b',1,1000,(0:127)*2.5,0,85)
    COR_uprog(2565824,1,vc+1,'s',1,3965,(0:127)*2.5,35,71)
end

COR_end
