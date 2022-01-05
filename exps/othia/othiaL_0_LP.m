N_SCAN=12;
shinv=0

COR_init((30+30+177+179+30+3)*N_SCAN,89)
for vc=1:3:N_SCAN*3
    i=(vc-1)/3;
    if shinv
    COR_uprog(12,1,vc+rem(i,3),'s',1,845+3*30,(0:74)*6,-3*75,1) %shinv
    else
    COR_uprog(12,1,vc+rem(i,3),'s',1,845,(0:74)*6,0,1)
    COR_uprog(60624,1,vc+rem(i+1,3),'s',1,845,(0:74)*6,0,1)
    end
    COR_fraclp(121224,vc+rem(i+1,3),'s',845-75*2,3,450,(1:89)*6,1)
    %COR_fdalt(290159,vc+rem(i+1,3),'s',0,0,2,3,450,836-60,(0:45)*12,1)
    COR_uprog(290876,1,vc+rem(i+2,3),'b',1,845,(0:74)*6,0,1)
    COR_pp(0,1,vc+rem(i,3),'c',1,12,0,1)
    COR_pp(60612,1,vc+rem(i+1,3),'c',1,12,0,1)
    COR_pp(290864,1,vc+rem(i+2,3),'c',1,12,0,1)
end
   
COR_end
