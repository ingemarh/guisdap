N_SCAN=64;

COR_init(513*N_SCAN,43)
for vc=1:N_SCAN
    COR_fraclp(728,vc,'s',319,16,36,(1:29)*12,1)
    COR_pp(rem(vc,2)*364,1,vc,'s',1,364,0,1)
    COR_pp(17498,1,vc,'c',1,15,0,1)
    COR_pp(17134,1,vc,'b',1,364,0,1)
%   COR_pp(17513,1,vc+64,'s',1,200,0,2)
%   COR_pp(17731,1,vc+64,'c',1,18,0,2)
%   COR_pp(17713,1,vc+64,'b',1,18,0,2)
end
   
COR_end
