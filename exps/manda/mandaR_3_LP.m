N_SCAN=length(vc_ch);

COR_init(N_SCAN*(129+1+1),127)
for vc=1:N_SCAN
  COR_pp(1000+2*(21+1)+128*21,1,vc,'s',1,2*21+128-2,0,1)
  COR_fdalt(1000,vc,'s',21,0,0,64,4,0,(0:127)*2,1)
  if rem(vc,2)==1
    COR_pp(500,1,vc,'c',1,500,0,1)
  else
    COR_pp(0,1,vc,'b',1,500,0,1)
  end
end

COR_end
