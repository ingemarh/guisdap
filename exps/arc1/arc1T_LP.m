N_SCAN=length(vc_ch);

COR_init(N_SCAN*((2*4-1)*16+1+1),60)
for vc=1:N_SCAN
  % The ac pulses
    vcc=fix((vc-1)/128);
    COR_arclp(3834+vcc*5808,vc,'s',363,64,4,128,1,0,(0:15)*24,vcc+1);
    COR_pp(vcc*426,1,vc,'s',1,426,0,vcc+1)
end
for vc=1:2:N_SCAN
    vcc=fix((vc-1)/128);
    COR_pp(56106,1,vc,'b',1,20,0,vcc+1)
end
for vc=2:2:N_SCAN
    vcc=fix((vc-1)/128);
    COR_pp(56126,1,vc,'c',1,20,0,vcc+1)
end
   
COR_end
