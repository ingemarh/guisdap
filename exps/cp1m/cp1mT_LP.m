N_SCAN=length(vc_ch);

COR_init((30+30+1+597+1+1+1)*N_SCAN,43)  %still from cp1l
%for vc=0:10:N_SCAN-1;                   % from cp1l
for vc=0:8:N_SCAN-1;
 for s=1:2
% COR_uprog(ra,ri,vc,type,gating,N_gates,lags,N_skipped,code) undecoded lag profiles
  COR_uprog( 1878,1,vc+s,'s',1,476,0:10:240,0,1)     %ch2 in cp1mT.DECO, LP
  COR_uprog(13478,1,vc+s,'b',1,202,0:10:240,0,1)     %ch2 LP
% COR_pp(ra,ri,vc,type,gating,N_gates,N_skipped,code)   power profiles
  COR_pp(18228,1,vc+s,'c',1,26,0,1)                  %ch2 LP
  COR_pp(18254,1,vc+2+s,'s',1,367,0,2)               %ch3 in cp1mT.DECO, ac zero lag 
% COR_fraclp(ra,vc,type,N_gates,N_bits,bitstep,lags,code)  alt code lag profiles a la EISCAT
  COR_fraclp(18621,vc+4+s,'s',298,16,21,(1:44)*7,2)  %ch3 ac
  COR_pp(41655,1,vc+6+s,'s',1,367,0,2)               %ch3 in cp1mT.DEC, pp
  COR_pp(42022,1,vc+6+s,'b',1,276,0,2)               %ch3 in cp1mT.DEC, pp
  COR_pp(42298,1,vc+6+s,'c',1,39,0,2)                %ch3 in cp1mT.DEC, pp
 end
end
   
COR_end
