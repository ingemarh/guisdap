N_SCAN=length(vc_ch);

COR_init(N_SCAN*507,43)
%lp_fir=zeros(43,5000);
for vc=1:N_SCAN;
  % The ac pulses
    %COR_fraclp(64,vc,'s',19,16,36,(1:23)*12,1)
    COR_fraclp(64,vc,'s',19,16,36,(1:29)*12,1)
end
for vc=1:N_SCAN;
    COR_pp(0,1,vc,'s',1,64,0,1)
    %COR_pp(1967,1,vc,'c',1,64,0,1)
    COR_pp(2340,1,vc,'c',1,64,0,1)
    %COR_pp(2031,1,vc,'b',1,256,0,1)
    COR_pp(2404,1,vc,'b',1,256,0,1)
    %COR_pp(2287,1,vc,'c',1,64,0,1)
    COR_pp(2660,1,vc,'c',1,64,0,1)
end
   
COR_end
