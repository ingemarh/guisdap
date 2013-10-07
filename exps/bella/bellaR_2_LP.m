N_SCAN=64;

COR_init((435+1+1)*N_SCAN,29)
for vc=1:N_SCAN
  COR_fraclp(288+148,vc,'s',148-29,30,45,(1:29)*45,1)
  COR_pp(288,1,vc,'s',1,148,0,1)
end

for vc=1:2:N_SCAN
  COR_pp(0,1,vc,'b',1,96,0,1)
end
for vc=2:2:N_SCAN
  COR_pp(96,1,vc,'c',1,96,0,1)
end
   
COR_end
