N_SCAN=64;

COR_init((34545+2)*N_SCAN,3749)
for vc=1:N_SCAN
    COR_fdalt(20,vc,'s',3,15,0,30,50,0,(0:2048)*.4,1)
    COR_pp(39509,1,vc,'s',1,4000,0,1)
end
for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
end
   
COR_end
