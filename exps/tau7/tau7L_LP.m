N_SCAN=32;

COR_init((2791+2729+1+1+3/2)*N_SCAN,15*24)
for vc=1:N_SCAN
    COR_fraclp(1446,vc,'s',1416-24*15,16,120,(1:119)*5,1)
    COR_fdalt(286458,vc,'s',0,0,14,16,120,1056,(0:192)*5,1)
    COR_pp(30,1,vc,'s',1,1416,0,1)
    COR_pp(289526,1,vc+N_SCAN,'b',1,1416,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
    COR_pp(289516,1,vc+N_SCAN,'c',1,10,0,2)
end
   
COR_end
