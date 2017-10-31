N_SCAN=32;

COR_init((360+213+1+1+3/2)*N_SCAN,31)
for vc=1:N_SCAN
    COR_fraclp(174,vc,'s',144-2*15,16,50,(1:31)*25,1)
    COR_fdalt(5934,vc,'s',0,0,14,16,50,114,(0:15)*25,1)
    COR_pp(30,1,vc,'s',1,144,0,1)
    %COR_pp(6432,1,vc+N_SCAN,'b',1,144,0,2) %first 5min
    COR_pp(6208,1,vc+N_SCAN,'b',1,144,0,2)
end

for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
    %COR_pp(6422,1,vc+N_SCAN,'c',1,10,0,2) %first 5min
    COR_pp(6198,1,vc+N_SCAN,'c',1,10,0,2)
end
   
COR_end
