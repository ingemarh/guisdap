N_SCAN=64;

COR_init((464+1+1+1+1)*N_SCAN,29)
for vc=1:N_SCAN
    COR_fdalt(48,vc,'s',192-29,28,0,30,45,0,(0:30)*45,1)
    COR_pp(5970,1,vc,'s',1,192,0,1)
    COR_pp(6162+32,1,vc+N_SCAN,'b',1,192,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,16,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(16,1,vc,'c',1,16,0,1)
    COR_pp(6162+16,1,vc+N_SCAN,'c',1,16,0,2)
end
   
COR_end
