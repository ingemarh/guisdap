N_SCAN=length(vc_ch);

COR_init(N_SCAN*478*4,43)
for vc=1:2:N_SCAN;
    COR_pp(0,1,vc,'s',1,366,0,1)
    COR_fraclp(366,vc,'s',366-45,16,60,(1:26)*20,1)
%   COR_pp(15147,1,vc,'c',1,12,0,1)
    COR_pp(15159,1,vc,'b',1,366,0,1)
    COR_pp(15525,1,vc,'c',1,12,0,1)
%
    COR_pp(15537,1,vc+1,'s',1,366,0,2)
    COR_fraclp(15903,vc+1,'s',366-45,16,60,(1:26)*20,2)
%   COR_pp(30684,1,vc+1,'c',1,12,0,2)
    COR_pp(30696,1,vc+1,'b',1,366,0,2)
    COR_pp(31062,1,vc+1,'c',1,12,0,2)
end
   
COR_end
