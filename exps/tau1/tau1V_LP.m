N_SCAN=length(vc_ch);

COR_init(N_SCAN*510,43)
for vc=1:2:N_SCAN
    vc1=vc+1;
    COR_fraclp(1092,vc,'s',501,16,72,(1:29)*24,1)
    COR_pp(rem(vc1/2,2)*546,1,vc,'s',1,546,0,1)
%   COR_pp(26022,1,vc,'c',1,21,0,1)
    COR_pp(26043,1,vc,'b',1,546,0,1)
    COR_pp(26589,1,vc,'c',1,21,0,1)

    COR_fraclp(26610+1092,vc1,'s',501,16,72,(1:29)*24,2)
    COR_pp(26610+rem(vc1/2,2)*546,1,vc1,'s',1,546,0,2)
%   COR_pp(26610+26022,1,vc1,'c',1,21,0,2)
    COR_pp(26610+26043,1,vc1,'b',1,546,0,2)
    COR_pp(26610+26589,1,vc1,'c',1,21,0,2)
end
   
COR_end
