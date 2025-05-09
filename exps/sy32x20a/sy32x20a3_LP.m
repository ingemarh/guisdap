COR_init
nocal=1;
for vc=1:64
 if false
  COR_fdalt(0,vc,'s',286,30,0,32,20,21,(0:64)*10,51)
  COR_uprog(317*2+316*64,1,vc,'s',1,634,0,21,51)
  COR_uprog(317*2+316*64+634,1,vc,'b',1,634,0,21,51)
 else
  COR_uprog(0,1,vc,'s',1,1000,0,21,51)
  COR_uprog(1000,1,vc,'b',1,268,0,21,51)
  COR_fraclp(1268,vc,'s',1000-2*31,32,20,(1:63)*10,51,21)
 end
end
COR_end
