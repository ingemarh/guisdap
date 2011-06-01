N_SCAN=length(vc_ch);

COR_init(N_SCAN*(7556+1+1),121)
for vc=1:N_SCAN
  COR_pp(20+(411+59+59+1)*2+120*(411+59+59),1,vc,'x',1,(411+61-1)*2,0,1)
  COR_fdalt(20,vc,'s',411,59,59,61,2.4,0,(0:120)*1.2,1)
  if rem(vc,2)==1
    COR_pp(10,1,vc,'c',1,10,0,1)
  else
    COR_pp(0,1,vc,'b',1,10,0,1)
  end
end

COR_end
