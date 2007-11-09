N_SCAN=length(vc_ch);

COR_init((3+600+19)*N_SCAN,47)
for vc=1:N_SCAN
    COR_uprog(0,1,vc,'s',1,300,(0:2)*28,0,1)
    COR_fraclp(897,vc,'s',100-45,16,84,(1:47)*28,1,50)
    COR_uprog(6597,1,vc,'b',1,21,(0:18)*28,0,1)
end

COR_end
nocal=1;
