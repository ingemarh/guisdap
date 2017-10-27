N_SCAN=length(vc_ch);
nocal=1;

COR_init
for vc=1:N_SCAN
 COR_fdalt(0,vc,'s',173-14,14,0,16,30,0,(0:16)*30,1)
 COR_pp(174+173*16+33,1,vc,'b',1,174-33,0,1)
end
   
COR_end
