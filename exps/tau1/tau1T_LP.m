N_SCAN=64;

COR_init(628*N_SCAN,43)
for vc=1:N_SCAN
    COR_fraclp(1456,vc,'s',728-5*15,16,60,(1:29)*12,1)
    COR_pp(rem(vc,2)*728,1,vc,'s',1,728,0,1)
    COR_pp(37086,1,vc,'c',1,15,0,1)
    COR_pp(36358,1,vc,'b',1,728,0,1)
end
   
COR_end
