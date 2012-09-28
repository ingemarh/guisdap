N_SCAN=64;

COR_init((11050+2)*N_SCAN,1599)
for vc=1:N_SCAN
    COR_fdalt(32061,vc,'s',78,12,0,32,20,0,(0:800)*.4,1)
    COR_pp(32061+91*50+90*800,1,vc,'s',1,5450,0,1)
end
for vc=1:2:N_SCAN
    COR_pp(32029,1,vc,'b',1,16,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(32029+16,1,vc,'c',1,16,0,1)
end
   
COR_end
