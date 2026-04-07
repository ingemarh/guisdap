N_SCAN=128;

COR_init((1179+732+1+1+3/2)*N_SCAN,63)
for vc=1:N_SCAN
    COR_fraclp(925,vc,'s',895-5*60,61,25,(1:144)*5,1)
    COR_fdalt(211205,vc,'s',0,0,59,61,25,800,(0:155)*5,1)
    COR_pp(30,1,vc,'s',1,895,0,1)
    COR_pp(230385,1,vc+N_SCAN,'b',1,895,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
    COR_pp(230395,1,vc+N_SCAN,'c',1,10,0,2)
end
   
COR_end
