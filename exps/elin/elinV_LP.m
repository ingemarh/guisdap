N_SCAN=length(vc_ch);

COR_init
for vc=1:N_SCAN;
 %OR_pp(38392,1,vc,'s',1,100,0,1)
 COR_pp(38392,1,vc,'s',1,79,0,1)
 %OR_fraclp(38492,vc,'s',100-10,3,200,(1:14)*40,1)
 COR_fraclp(38471,vc,'s',79-8,3,200,(1:11)*50,1)
 if rem(vc-1,4)>1
  %OR_pp(40159,1,vc,'c',1,2,0,1)
  COR_pp(39494,1,vc,'c',1,1,0,1)
 else
  %OR_pp(40157,1,vc,'b',1,2,0,1)
  COR_pp(39493,1,vc,'b',1,1,0,1)
 end
end
COR_end
