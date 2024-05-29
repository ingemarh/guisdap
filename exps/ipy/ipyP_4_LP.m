N_SCAN=64;

%COR_init((8998+2)*N_SCAN,1800)
if d_rcprog==4.1
  COR_init((19604+2)*N_SCAN,2249)
  for vc=1:N_SCAN
    COR_fdalt(20,vc,'o',41,15,10,30,30,0,(0:1152)*.4,1)
    COR_pp(81377,1,vc,'o',1,5250,0,1)
    for skip=(0:16)*75
      COR_trilp(81077,75,vc,'o',75,16*75,4,(0:74)*.4,2,skip)
    end
  end
  for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
  for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
  end
elseif d_rcprog==4.2
  COR_init((19604+2)*N_SCAN,2249)
  for vc=1:N_SCAN
    COR_fdalt(20,vc,'s',51,15,15,30,30,0,(0:1152)*.4,1)
    COR_pp(99857,1,vc,'s',1,6000,0,1)
    for skip=(0:14)*75
      COR_trilp(99482,75,vc,'o',75,14*75,5,(0:74)*.4,2,skip)
    end
  end
  for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
  for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
  end
end
   
COR_end
