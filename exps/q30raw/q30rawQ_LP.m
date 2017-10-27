N_SCAN=length(vc_ch);
nocal=1;

COR_init
for vc=1:N_SCAN
 COR_fdalt(0,vc,'s',261-14,14,0,16,20,0,(0:16)*20,1)
 COR_pp(262+261*16,1,vc,'b',1,262,0,1)
end
   
COR_end
