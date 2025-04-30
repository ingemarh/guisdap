COR_init
nocal=1;
for vc=1:64
COR_fdalt(0,vc,'s',286,30,0,32,20,21,(0:64)*10,51)
COR_uprog(317*2+316*64,1,vc,'s',1,634,0,21,51)
COR_uprog(317*2+316*64+634,1,vc,'b',1,634,0,21,51)
end
COR_end
