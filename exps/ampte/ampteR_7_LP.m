COR_init

for vc=1:4
%COR_pp(0+(vc-1)*30,1,vc,'s',1,30,0,1)
 COR_pp(0,1,vc,'x',1,30,0,1) %join channels
%COR_trilp(120,16,vc,'s',18,-16,7,0:10:150,1)
 COR_trilp(120+3*16,16,vc,'s',18,-16,1,0:10:150,1) %only middle gate
 COR_trilp(120+7*16,16,vc,'b',18,0,5,0:10:150,1)
 COR_trilp(120+12*16,16,vc,'c',18,0,5,0:10:150,1)
end

COR_end
