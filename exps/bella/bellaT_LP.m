N_SCAN=64;

COR_init((1263+1+1+1+.5)*N_SCAN,85)
for vc=1:N_SCAN
  COR_fraclp(663,vc,'s',615-3*29,30,45,(1:32)*15,1)
  COR_pp(48,1,vc,'s',1,615,0,1)
  COR_pp(31755+32,1,vc+N_SCAN,'b',1,615,0,2)
end

for vc=1:2:N_SCAN
  COR_pp(0,1,vc,'b',1,16,0,1)
end
for vc=2:2:N_SCAN
  COR_pp(16,1,vc,'c',1,16,0,1)
  COR_pp(31755+16,1,vc+N_SCAN,'c',1,16,0,2)
end
   
COR_end
