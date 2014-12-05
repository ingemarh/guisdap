N_SCAN=length(vc_ch)/2;

COR_init(round((32895+3.5)*N_SCAN),255)
for vc=1:N_SCAN
 COR_pp(456738,1,vc,'s',1,1778,0,1)
 COR_fdalt(48,vc,'s',1778-255,254,0,256,2,0,(0:256)*2,1)
 COR_pp(458548,1,vc+N_SCAN,'b',1,1778,0,2)
 if rem(vc-1,2)
  COR_pp(16,1,vc,'c',1,16,0,1)
  COR_pp(458532,1,vc,'c',1,16,0,2)
 else
  COR_pp(0,1,vc,'b',1,16,0,1)
  %OR_pp(458516,1,vc+N_SCAN,'b',1,16,0,2)
 end
end
COR_end
