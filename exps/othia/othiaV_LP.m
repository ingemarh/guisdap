N_SCAN=12;
shinv=0

COR_init((30+30+177+179+30+3)*N_SCAN,89)
for vc=1:3:N_SCAN*3
    i=(vc-1)/3;
    if shinv
    COR_uprog(12,1,vc+rem(i,3),'s',1,836+3*30,(0:29)*12,-3*30,1) %shinv
    else
    COR_uprog(12,1,vc+rem(i,3),'s',1,836,(0:29)*12,0,1)
    COR_uprog(24669,1,vc+rem(i+1,3),'s',1,836,(0:29)*12,0,1)
    end
    COR_fraclp(49314,vc+rem(i+1,3),'s',836-30*2,3,360,(1:89)*12,1)
    COR_fdalt(142652,vc+rem(i+1,3),'s',0,0,1,3,360,836-60,(0:45)*12,1)
    COR_uprog(142769,1,vc+rem(i+2,3),'b',1,836,(0:29)*12,0,1)
    COR_pp(0,1,vc+rem(i,3),'c',1,12,0,1)
    COR_pp(24657,1,vc+rem(i+1,3),'c',1,12,0,1)
    COR_pp(142757,1,vc+rem(i+2,3),'c',1,12,0,1)
end
   
COR_end
