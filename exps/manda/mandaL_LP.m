N_SCAN=length(vc_ch);

COR_init(N_SCAN*(3599+2079+1+1),63)
for vc=1:N_SCAN
  COR_pp(17441,1,vc,'x',1,236,0,1)
  COR_fdalt(20,vc,'s',173,32,63,64,5,0,(0:64)*5,1)
  COR_fdalt(23757,vc,'s',173,63,0,64,5,1875/5,(0:64)*5,1)
  if rem(vc,2)==1
    COR_pp(10,1,vc,'c',1,10,0,1)
  else
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
end

COR_end
