N_SCAN=length(vc_ch);

COR_init(N_SCAN*(2*6-1)*9,60-6)
for vc=1:N_SCAN;
  % The ac pulses
%   COR_arclp(0,vc,'s',46,60,6,128,(0:9)*24,1);
    COR_arclp(46,vc,'s',46,60,6,128,(1:9)*24,1);
end
for vc=1:2:N_SCAN;
    COR_pp(460+20*fix(vc/2),1,vc,'c',1,20,0,1)
end
for vc=2:2:N_SCAN;
    COR_pp(460+1280-20+20*fix(vc/2),1,vc,'b',1,20,0,1)
end
   
COR_end
