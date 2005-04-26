N_SCAN=length(vc_ch);

COR_init(N_SCAN*(2608+2080+1),63)
for vc=1:N_SCAN
  % The ac pulses
    COR_fdalt(20,vc,'s',355,32,63,64,3,0,(0:63)*3,1)
    %COR_pp(29271,1,vc,'s',1,418,0,1)
    COR_fdalt(42229,vc,'s',355,63,0,64,3,1875/3,(0:63)*3,1)
%COR_fdalt(ra,vc,type,N_gates,NlowG,NupG,Nbits,Sample_skip,lags,code)
end
for vc=1:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end

COR_end
