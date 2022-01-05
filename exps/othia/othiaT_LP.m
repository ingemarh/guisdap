N_SCAN=12;
shinv=0

COR_init((60+60+372+270+60+3)*N_SCAN,179)
for vc=1:3:N_SCAN*3
    i=(vc-1)/3;
    if shinv
    COR_uprog(12,1,vc+rem(i,3),'s',1,1700+3*30,(0:59)*4,-3*75,1) %shinv
    else
    COR_uprog(12,1,vc+rem(i,3),'s',1,1700,(0:59)*4,0,1)
    COR_uprog(100254,1,vc+rem(i+1,3),'s',1,1700,(0:59)*4,0,1)
    end
    COR_fraclp(200484,vc+rem(i+1,3),'s',1700-60*2,3,240,(1:179)*4,1)
    COR_fdalt(583664,vc+rem(i+1,3),'s',0,0,1,3,240,1700-120,(0:90)*4,1)
    COR_uprog(583886,1,vc+rem(i+2,3),'b',1,1700,(0:59)*4,0,1)
    COR_pp(0,1,vc+rem(i,3),'c',1,12,0,1)
    COR_pp(100254-12,1,vc+rem(i+1,3),'c',1,12,0,1)
    COR_pp(583886-12,1,vc+rem(i+2,3),'c',1,12,0,1)
end
   
COR_end
