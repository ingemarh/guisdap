N_SCAN=64;

COR_init((1301+1+1+1)*N_SCAN,63)
for vc=1:N_SCAN
    COR_fraclp(1918+143,vc,'s',72-2*31,32,20,(1:41)*10,1)
    COR_uprog(1918,1,vc,'s',1,72,[0 10],0,1)
end

for vc=1:2:N_SCAN
    COR_uprog(0,1,vc,'b',1,384,[0 10],0,1)
end
for vc=2:2:N_SCAN
    COR_uprog(767,1,vc,'c',1,384,[0 10],0,1)
end
   
COR_end
