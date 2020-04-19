N_SCAN=length(vc_ch);

COR_init
COR_init(N_SCAN*(2945+1+1),127)
for vc=1:N_SCAN
 COR_fdalt(14,vc,'s',1190/2-63,48,0,64,30,0,(0:64)*15,1)
 COR_pp(14+581*2+580*64,1,vc,'s',1,1190,0,1)
 if rem(vc,2)
  COR_pp(0,1,vc,'c',1,7,0,1)
 else
  COR_pp(7,1,vc,'b',1,7,0,1)
 end
end
   
COR_end
