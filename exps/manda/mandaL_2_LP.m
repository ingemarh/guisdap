N_SCAN=length(vc_ch);

COR_init(N_SCAN*(3074+3479+1769+1+1),58)
for vc=1:N_SCAN
  COR_pp(20+144+60*143,1,vc,'x',1,114,0,1)
  COR_fdalt(20,vc,'s',56,30,57,59,5,0,(0:58)*5,1)
  COR_fdalt(15508,vc,'s',56,57,57,59,5,1250/5,(0:58)*5,1)
  COR_fdalt(25879,vc,'s',56,57,0,59,5,2500/5,(0:58)*5,1)
  if rem(vc,2)==1
    COR_pp(10,1,vc,'c',1,10,0,1)
  else
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
end

COR_end
