N_SCAN=length(vc_ch);

COR_init
for vc=1:N_SCAN;
 COR_pp(0,1,vc,'s',1,206,0,1)
 COR_fraclp(206,vc,'s',206-20,3,100,(1:29)*10,1)
 if vc>4
  COR_pp(7472,1,vc,'c',1,8,0,1)
 else
  COR_pp(7464,1,vc,'b',1,8,0,1)
 end
end
   
COR_end
