N_SCAN=64;

COR_init((1263+3+3)*N_SCAN,85)
for vc=1:N_SCAN
  COR_fraclp(4768,vc,'s',99-3*29,30,45,(1:32)*15,1)
  COR_uprog(4474,1,vc,'s',1,99,[0 15 30],0,1)
end

for vc=1:2:N_SCAN
  COR_uprog(0,1,vc,'b',1,640,[0 15 30],0,1)
end
for vc=2:2:N_SCAN
  COR_uprog(1917,1,vc,'c',1,640,[0 15 30],0,1)
end
   
COR_end
