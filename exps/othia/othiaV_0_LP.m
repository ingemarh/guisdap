N_SCAN=12;

COR_init((30+355+30+3)*N_SCAN,90)
for vc=1:3:N_SCAN*3
    i=(vc-1)/3;
    COR_uprog(12,1,vc+rem(i,3),'s',1,840,(0:29)*12,0,1)
%   COR_trilp(100000,30,vc+rem(i,3),'s',30,0,28+3,(0:29)*12,1) %after shinv
%   COR_lp(200000,30,vc+rem(i,3),'s',0,30,27,(1:29)*12,1)    %after shinv
    COR_fdalt(24789,vc+rem(i+1,3),'s',26,2,2,3,360,0,(0:90)*12,1)
    COR_uprog(30111,1,vc+rem(i+2,3),'b',1,840,(0:29)*12,0,1)
    COR_pp(0,1,vc+rem(i,3),'c',1,12,0,1)
    COR_pp(24777,1,vc+rem(i+1,3),'c',1,12,0,1)
    COR_pp(30099,1,vc+rem(i+2,3),'c',1,12,0,1)
end
   
COR_end
