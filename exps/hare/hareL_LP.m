N_SCAN=length(vc_ch);

COR_init
for vc=1:N_SCAN;
 COR_pp(30424,1,vc,'s',1,8712,0,1) % from RTG_def
 COR_fdalt(0,vc,'s',64,1,0,3,660,0,(0:200)*5,1)
 COR_pp(39136+8712,1,vc,'c',1,134,0,1)
 COR_pp(39136,1,vc,'b',1,8712,0,1)
end
COR_end
