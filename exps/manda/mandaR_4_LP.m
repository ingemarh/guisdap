N_SCAN=length(vc_ch);

COR_init(N_SCAN*(61+1+1),60)
for vc=1:N_SCAN
  COR_pp(1000+(53+1)+60*53,1,vc,'x',1,53+61-1,0,1)
  COR_fdalt(1000,vc,'s',53,0,0,61,2.4,0,(0:60)*2.4,1)
  if rem(vc,2)==1
    COR_pp(500,1,vc,'c',1,500,0,1)
  else
    COR_pp(0,1,vc,'b',1,500,0,1)
  end
end

COR_end
