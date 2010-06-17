N_SCAN=64;

%COR_init((25619+3)*N_SCAN,999)
COR_init((8998+2)*N_SCAN,1800)
for vc=1:N_SCAN
    COR_fdalt(20,vc,'s',23,10,0,30,30,0,(0:768)*.6,1)
    COR_pp(27214,1,vc,'s',1,2600,0,1)
end
for vc=1:2:N_SCAN
    COR_pp(0,1,vc,'b',1,10,0,1)
end
for vc=2:2:N_SCAN
    COR_pp(10,1,vc,'c',1,10,0,1)
end
   
COR_end
