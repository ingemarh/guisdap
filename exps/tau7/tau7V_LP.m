N_SCAN=length(vc_ch);

COR_init(N_SCAN*(1188+8+1+8+1),15*8-7)
for vc=1:N_SCAN;
  % The ac pulses
    COR_fraclp(8668,vc,'s',1087-8*15,16,8*12,(1:55)*12,1)
    COR_uprog(0,1,vc,'s',1,1087,(0:7)*12,0,1)
    COR_pp(111223,1,vc,'c',1,21,0,1)
    COR_uprog(111244,1,vc,'b',1,1087,(0:7)*12,0,1)
    COR_pp(119912,1,vc,'c',1,21,0,1)
end
   
COR_end
