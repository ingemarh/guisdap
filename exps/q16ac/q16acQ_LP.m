N_SCAN=length(vc_ch);
nocal=1;

COR_init
for vc=1:N_SCAN
 COR_fdalt(0,vc,'s',150,0,0,16,30,0,0,1)
 COR_pp(151+151*16,17,vc,'s',1,141,0,1)
 COR_alter(151+151*16+(1:15),17,vc,'s',1,151,16,30,(1:15)*30,0,1)
%COR_alter(151+(1:15),16,vc,'s',1,151,16,30,(1:15)*30,0,1)
 COR_pp(151+151*16+17*141,17,vc,'b',1,10,0,1)
end
   
COR_end
