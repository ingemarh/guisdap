N_SCAN=length(vc_ch);

COR_init(N_SCAN*((2*4-1)*16+1+1),60)
for vc=1:N_SCAN;
  % The ac pulses
    COR_arclp(505,vc,'s',442,64,4,128,1,0,(0:15)*24,1);
    COR_pp(0,1,vc,'s',1,505,0,1)
end
for vc=1:2:N_SCAN;
    COR_pp(7577,1,vc,'b',1,20,0,1)
end
for vc=2:2:N_SCAN;
    COR_pp(7597,1,vc,'c',1,20,0,1)
end
   
COR_end
