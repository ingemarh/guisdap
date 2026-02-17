N_SCAN=length(vc_ch);

nocal=1;
COR_init(N_SCAN*(1+537+285),32)
nskip=288;
nsamp=(4188-nskip)/6;
for vc=1:N_SCAN
  COR_pp(11494,1,vc,'b',1,nsamp,0,48)
  COR_fdalt(0,vc,'s',nsamp/2-15,14,14,16,3,nskip/6,(0:32)*1.5,48)
  COR_fdalt(11494+nsamp+200*(31+1),vc,'s',nsamp/2-15,14,0,16,3,nskip/6+1250/1.5,(0:32)*1.5,48)
  %COR_pp(152316,1,vc,'b',1,1000,0,1)
end

COR_end
