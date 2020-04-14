N_SCAN=length(vc_ch);

COR_init
for vc=1:N_SCAN
 COR_fdalt(200,vc,'s',1192-2*62,62,0,64,30,0,(0:256)*15,1)
 COR_pp(0,1,vc,'b',1,100,0,1)
 COR_pp(100,1,vc,'c',1,100,0,1)
end
   
COR_end
