N_SCAN=length(vc_ch);

COR_init
for vc=1:N_SCAN;
  % The ac pulses
    %COR_fraclp(364,vc,'s',319,16,36,(1:23)*12,1)
    COR_fraclp(728,vc,'s',319,16,36,(1:29)*12,1)
    COR_pp(0,1,vc,'s',1,364,0,1)
    %COR_pp(13367,1,vc,'c',1,15,0,1)
    COR_pp(17104,1,vc,'c',1,15,0,1)
    %COR_pp(13746,1,vc,'c',1,15,0,1)
    COR_pp(17483,1,vc,'c',1,15,0,1)
    %COR_pp(13382,1,vc,'b',1,364,0,1)
    COR_pp(17119,1,vc,'b',1,364,0,1)
end
   
COR_end
