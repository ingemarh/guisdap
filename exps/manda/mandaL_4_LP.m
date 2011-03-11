N_SCAN=length(vc_ch);

COR_init(N_SCAN*(8061+8313+1+1),127)
for vc=1:N_SCAN
  COR_pp(20+2*(101+48+62+1)+128*(101+48+62),1,vc,'x',1,2*101+128-2,0,1)
  COR_fdalt(20,vc,'s',101,48,62,64,4,0,(0:127)*2,1)
  COR_fdalt(42080,vc,'s',101,62,62,64,4,1250/2,(0:127)*2,1)
  if rem(vc,2)==1
    COR_pp(10,1,vc,'c',1,10,0,1)
  else
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
end

COR_end
