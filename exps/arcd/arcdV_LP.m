N_SCAN=length(vc_ch);

COR_init(N_SCAN*((2*64-1)*1+(2*16-1)*4+1+1),60)
for vc=1:N_SCAN
  % The ac pulses
    COR_arclp(420,vc,'s',267,64,64,128,1,0,0*2,1);
    COR_arclp(34596,vc,'s',267,64,16,128,1,0,(0:3)*2*16,1);
    COR_pp(90,1,vc,'x',1,330,0,1)
end
for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,45,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(45,1,vc,'c',1,45,0,1)
end

COR_end
