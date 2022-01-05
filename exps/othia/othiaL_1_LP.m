N_SCAN=12;
shinv=0

COR_init((75+75+372+270+75+3)*N_SCAN,149)
for vc=1:3:N_SCAN*3
    i=(vc-1)/3;
    if shinv
    COR_uprog(12,1,vc+rem(i,3),'s',1,875+3*30,(0:74)*6,-3*75,1) %shinv
    else
    COR_uprog(12,1,vc+rem(i,3),'s',1,875,(0:74)*6,0,1)
    COR_uprog(62874,1,vc+rem(i+1,3),'s',1,875,(0:74)*6,0,1)
    end
    COR_fraclp(125724,vc+rem(i+1,3),'s',875-75*2,3,450,(1:149)*6,1)
    COR_fdalt(301349,vc+rem(i+1,3),'s',0,0,1,3,450,875-150,(0:120)*6,1)
    COR_uprog(301631,1,vc+rem(i+2,3),'b',1,875,(0:74)*6,0,1)
    COR_pp(0,1,vc+rem(i,3),'c',1,12,0,1)
    COR_pp(62874-12,1,vc+rem(i+1,3),'c',1,12,0,1)
    COR_pp(301631-12,1,vc+rem(i+2,3),'c',1,12,0,1)
end
   
COR_end
