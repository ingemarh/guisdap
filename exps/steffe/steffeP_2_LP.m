N_SCAN=32;

COR_init((8555+3)*N_SCAN,2799)
for vc=1:N_SCAN
    COR_fdalt(0,vc,'s',5,4,0,16,105,0,(0:1536)*.6,1)
    COR_pp(16099,1,vc,'s',1,3500,0,1)
    COR_pp(19599,1,vc,'b',1,10,0,1)
    COR_pp(19609,1,vc,'c',1,10,0,1)
end
   
COR_end
