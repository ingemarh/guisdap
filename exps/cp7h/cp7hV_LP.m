N_SCAN=length(vc_ch);

COR_init
for vc=1:3:N_SCAN;
    COR_uprog(    0,1,vc  ,'s',1,928,(0:29)*15,0,1)
    COR_uprog(27405,1,vc+1,'s',1,928,(0:29)*15,0,1)
    COR_pp(54810,1,vc+2,'s',1,166,0,1)
    COR_uprog(54976,1,vc+1,'b',1,348,(0:29)*15,0,1)
    COR_pp(64981,1,vc+1,'c',1,58,0,1)
end
   
COR_end
