N_SCAN=length(vc_ch);
nocal=1;
ng=151; nb=30;

COR_init
for vc=1:N_SCAN
 COR_fdalt(0,vc,'s',ng-1,0,0,16,nb,0,0,1)
 COR_alter(ng+(1:15),16,vc,'s',1,ng,16,nb,(1:15)*nb,0,1)
 COR_pp(ng+ng*16+17*ng,1,vc,'b',1,1,0,1)
end
   
COR_end
