N_SCAN=length(vc_ch);

COR_init((30+30+1+597+1+1+1)*N_SCAN,43)
for vc=0:10:N_SCAN-1;
 for s=1:2
  COR_uprog(  387,1,vc+s,'s',1,416,0:10:240,0,1)
  COR_uprog(10487,1,vc+s,'b',1,202,0:10:240,0,1)
  COR_pp(15237,1,vc+s,'c',1,26,0,1)
  COR_pp(15263,1,vc+2+s,'s',1,309,0,2)
  COR_fraclp(15572,vc+4+s,'s',240,16,21,(1:44)*7,2)
  COR_pp(34430,1,vc+6+s,'s',1,309,0,2)
  COR_pp(34739,1,vc+6+s,'b',1,276,0,2)
  COR_pp(35015,1,vc+6+s,'c',1,39,0,2)
  COR_pp(0,1,vc+8+s,'s',1,240,0,3)
  COR_pp(240,1,vc+8+s,'b',1,120,0,3)
  COR_pp(360,1,vc+8+s,'c',1,27,0,3)
 end
end
   
COR_end
