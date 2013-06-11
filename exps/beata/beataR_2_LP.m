N_SCAN=64;

COR_init((496+1+1)*N_SCAN,31)
for vc=1:N_SCAN
    COR_fraclp(675+42,vc,'s',42-31,32,20,(1:31)*20,1)
    COR_pp(675,1,vc,'s',1,42,0,1)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,225,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(225,1,vc,'c',1,225,0,1)
end
   
COR_end
