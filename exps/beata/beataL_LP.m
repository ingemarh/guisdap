N_SCAN=64;

COR_init((1179+1+1+15*29)*N_SCAN,63)
for vc=1:N_SCAN
  COR_fraclp(192,vc,'s',162-2*29,30,50,(1:41)*25,1)
  COR_pp(30,1,vc,'s',1,162,0,1)
  COR_pp(11208,1,vc+N_SCAN,'b',1,162,0,2)
  for i=0:14
    COR_fraclp(8773+i*161,vc,'s',162-2*29,30,50,25,i+3)
  end
end

for vc=1:2:N_SCAN
  COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
  COR_pp(10,1,vc,'c',1,10,0,1)
  COR_pp(11198,1,vc+N_SCAN,'c',1,10,0,2)
end
   
COR_end
