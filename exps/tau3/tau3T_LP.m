N_SCAN=length(vc_ch);

COR_init(N_SCAN*511,43)
for vc=1:N_SCAN;
  % The ac pulses
    COR_fraclp(728*2,vc,'s',683,16,36,(1:29)*12,1)
    COR_pp(0,1,vc,'s',1,364*2,0,1)
    COR_pp(34940,1,vc,'c',1,15,0,1)
    COR_pp(34955,1,vc,'b',1,364*2,0,1)
    COR_pp(35683,1,vc,'c',1,15,0,1)
end
   
COR_end
