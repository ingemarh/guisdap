N_SCAN=length(vc_ch);

COR_init(N_SCAN*(2*6-1)*10,60)
for vc=1:N_SCAN;
  % The ac pulses
    COR_arclp(0,vc,'s',637,60,6,128,(0:9)*24,1)
end
for vc=1:2:N_SCAN;
    COR_pp(6370+20*fix(vc/2),1,vc,'c',1,20,0,1)
end
for vc=2:2:N_SCAN;
    COR_pp(6370+1260+20*fix(vc/2),1,vc,'b',1,20,0,1)
end
   
COR_end
