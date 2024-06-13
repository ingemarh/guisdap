N_SCAN=length(vc_ch);

COR_init(N_SCAN*(10889+21448+95*10+2),639)
for vc=1:N_SCAN
  COR_pp(52520,1,vc,'o',1,1900,0,1)
  COR_fdalt(20,vc,'o',127,0,32,64,4,0,(0:320)*.4,1)
  COR_fdalt(54420,vc,'o',127,32,32,64,4,1250/.4,(0:320)*.4,1)
  for skip=(0:94)*10
    COR_trilp(52500,10,vc,'o',10,94*10,2,(0:9)*.4,2,skip)
  end
  if rem(vc,2)==1
    COR_pp(10,1,vc,'c',1,10,0,1)
  else
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
end

COR_end
