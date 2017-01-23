N_SCAN=length(vc_ch),

COR_init(N_SCAN*(111+1+1),64)
for vc=0:N_SCAN-1
  % The ac pulses
    vcc=fix((vc)/128);
    COR_arclp(11000+vcc*6032,vc+1,'s',377,64,4,128,1,0,(0:15)*16,vcc+1);
    COR_pp(vcc*440,1,vc+1,'s',1,440,0,vcc+1)
    COR_pp(161800+rem(vc,2)*20,1,vc+1,'b'+rem(vc,2),1,20,0,vcc+1)
end

COR_end
