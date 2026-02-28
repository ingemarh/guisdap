N_SCAN=length(vc_ch);

nocal=1;
COR_init(N_SCAN*(1+537+285),32)
nskip=516;
nsamp=(4188-nskip)/6;
for vc=1:N_SCAN
  COR_fdalt(0,vc,'s',nsamp/2-15,14,14,16,3,nskip/6+1,(0:32)*1.5,48)
  n=(nsamp/2-15+2*14)*(2+32)+2;
  COR_pp(n,1,vc,'x',1,nsamp,0,48)
  n=n+nsamp+100*(31+1);
  COR_pp(n,1,vc,'b',1,nsamp,0,48)
  COR_fdalt(n+nsamp,vc,'s',nsamp/2-15,14,0,16,3,nskip/6+1+1250/1.5,(0:32)*1.5,48)
end

COR_end
