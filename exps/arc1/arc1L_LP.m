N_SCAN=length(vc_ch);

COR_init(N_SCAN*((2*4-1)*16+1+1),64)
for vc=0:N_SCAN-1
  % The ac pulses
    vcc=fix(vc/128);
    COR_arclp(5050+vcc*7072,vc+1,'s',442,64,4,128,1,0,(0:15)*24,vcc+1);
    COR_pp(vcc*505,1,vc+1,'s',1,505,0,vcc+1)
    COR_pp(75770+rem(vc,2)*20,1,vc+1,'b'+rem(vc,2),1,20,0,1)
end
COR_end
