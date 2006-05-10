N_SCAN=64;

COR_init((1179+1+1)*N_SCAN,63)
for vc=1:N_SCAN
    COR_fraclp(192,vc,'s',162-2*29,30,50,(1:41)*25,1)
    COR_pp(30,1,vc,'s',1,162,0,1)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
end
   
COR_end
