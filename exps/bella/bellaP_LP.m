N_SCAN=64;

COR_init((1263+1+1+1+.5)*N_SCAN,85)
for vc=1:N_SCAN
  COR_trilp(40114+(1:74)*4,1,vc,'s',1536,0,1,(1:74)*0.6,1)
  COR_pp(32434,1,vc,'s',1,7680,0,1)
end

for vc=1:2:N_SCAN
  COR_pp(32402,1,vc,'b',1,16,0,1)
end
for vc=2:2:N_SCAN
  COR_pp(32418,1,vc,'c',1,16,0,1)
end
   
COR_end
