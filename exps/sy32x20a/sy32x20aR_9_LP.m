COR_init
nocal=1;
for vc=1:64
COR_fdalt(0,vc,'s',60,0,0,32,20,0,(0:64)*10,51)
COR_uprog(61*2+60*64,1,vc,'s',1,182,[0 10],0,51)
COR_uprog(61*2+60*64+182+181,1,vc,'b',1,182,[0 10],0,51)
end
COR_end
