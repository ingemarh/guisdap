N_SCAN=length(vc_ch);

COR_init((30+30+1+597+1+1+1)*N_SCAN,43)
for vc=0:6:N_SCAN-1;
 for s=1:2
  COR_uprog(    0,1,vc+s,'s',1,120,0:10:290,0,1)
  COR_uprog( 3165,1,vc+s,'b',1,378,0:10:290,0,1)
  COR_pp(14070,1,vc+s,'c',1,199,0,1)
  COR_uprog(14269,1,vc+s,'b',1,378,0:10:290,0,1)
  COR_pp(25174,1,vc+s,'c',1,199,0,1)

  COR_pp(25373,1,vc+2+s,'s',1,66,0,2)
  COR_fraclp(25439,vc+2+s,'s',21,16,21,(1:44)*7,2)
  COR_pp(28529,1,vc+4+s,'s',1,100,0,2)
  COR_pp(28529+100,1,vc+4+s,'b',1,542-100,0,2)
  COR_pp(29071,1,vc+4+s,'c',1,290,0,2)
 end
end
   
COR_end
