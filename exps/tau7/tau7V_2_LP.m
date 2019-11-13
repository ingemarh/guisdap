N_SCAN=32;

COR_init((1185+1+1+1+1)*N_SCAN,129)
for vc=1:N_SCAN
    COR_fdalt(48,vc,'s',896/8-15,14,0,16,120,0,(0:128)*15,1)
    COR_pp(15152,1,vc,'s',1,896,0,1)
    COR_pp(16080,1,vc+N_SCAN,'b',1,896,0,2)
end
for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,16,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(16,1,vc,'c',1,16,0,1)
    COR_pp(16064,1,vc+N_SCAN,'c',1,16,0,2)
end
   
COR_end
