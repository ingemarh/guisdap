N_SCAN=length(vc_ch);

COR_init(N_SCAN*440,43)
for vc=1:N_SCAN;
  % The ac pulses
    COR_fraclp(546,vc,'s',501,16,72,(1:23)*24,1)
    COR_pp(0,1,vc,'s',1,546,0,1)
    COR_pp(20283,1,vc,'c',1,21,0,1)
    COR_pp(20304,1,vc,'b',1,546,0,1)
    COR_pp(20850,1,vc,'c',1,21,0,1)
end
   
COR_end
