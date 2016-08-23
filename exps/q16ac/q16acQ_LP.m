N_SCAN=length(vc_ch);
nocal=1;

COR_init
for vc=1:N_SCAN;
 %COR_pp(0,1,vc,'s',1,151,0,1)
 COR_pp(151,16,vc,'x',1,151-10,0,1)
 COR_alter(151+151*16+(1:15),17,vc,'s',1,151,16,30,(1:15)*30,0,1)
%if vc>4
% COR_pp(7472,1,vc,'c',1,8,0,1)
%else
  COR_pp(151+16*141,16,vc,'b',1,10,0,1)
%end
end
   
COR_end
