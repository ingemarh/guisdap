N_SCAN=length(vc_ch)/2;

COR_init(round((29077+3.5)*N_SCAN),255)
for vc=1:N_SCAN
 COR_pp(359401,1,vc,'s',1,1657,0,1)
 COR_fdalt(48,vc,'s',1657-241,240,0,242,3,0,(0:216)*3,1)
 COR_pp(361090,1,vc+N_SCAN,'b',1,1657,0,2)
 if rem(vc-1,2)
  COR_pp(16,1,vc,'c',1,16,0,1)
  COR_pp(361058+16,1,vc,'c',1,16,0,2)
 else
  COR_pp(0,1,vc,'b',1,16,0,1)
 end
end
COR_end
