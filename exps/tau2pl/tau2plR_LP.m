N_SCAN=length(vc_ch);

COR_init(N_SCAN*513,43)
for vc=1:N_SCAN;
  % The ac pulses
    COR_fraclp(189,vc,'s',19,16,36,(1:29)*12,1)
    COR_uprog(0,1,vc,'s',1,64,(0:2)*12,0,1)
    COR_pp(2465,1,vc,'c',1,64,0,1)
    COR_uprog(2529,1,vc,'b',1,256,(0:2)*12,0,1)
    COR_pp(3294,1,vc,'c',1,64,0,1)
end
   
COR_end
