N_SCAN=128;

COR_init((7033+1883+1+1+3/2)*N_SCAN,180)
adci=25/3;
for vc=1:N_SCAN
    COR_fraclp(564,vc,'s',528-3*60,61,25,(1:95)*adci,1)
    COR_fdalt(75831,vc,'s',0,0,59,61,25,528-3*59,(0:31)*adci,1)
    COR_pp(36,1,vc,'s',1,528,0,1)
    COR_pp(87339,1,vc+N_SCAN,'b',1,528,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,12,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(12,1,vc,'c',1,12,0,1)
    COR_pp(87339,1,vc+N_SCAN,'c',1,12,0,2)
end
   
COR_end
