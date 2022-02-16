COR_init
nocal=1;
for vc=1:32
COR_fdalt(0,vc,'s',231,14,0,16,30,0,(0:47)*10,32)
COR_uprog(246*3+245*48,1,vc,'s',1,640,0,0,32)
COR_uprog(246*3+245*48+640,1,vc,'b',1,100,0,0,32)
end
COR_end
