N_SCAN=length(vc_ch);

COR_init(N_SCAN*(78+79+75+1),40)
for vc=1:N_SCAN
  % The ac pulses
%   COR_arclp(0,vc,'s',111,40,20,29.66290./taper(20),0,0,(0:1)*80,1);
    COR_arclp(111,vc,'s',111,40,20,29.66290./taper(20),0,0,80,1);
%   COR_arclp(2*111,vc,'s',111,40,40,19.05853./taper(40),0,0,0,1);
%   COR_arclp((2+31+25)*111,vc,'s',111,40,8,24.96932./taper(8),0,0,(0:4)*32,1);
    COR_arclp((2+31+25+1)*111,vc,'s',111,40,8,24.96932./taper(8),0,0,(1:4)*32,1);
end
for vc=0:2:N_SCAN-2
    COR_pp((2+31+25+5)*111+vc/2*15,1,vc+1,'c',1,15,0,1)
    COR_pp((2+31+25+5)*111+750+vc/2*15,1,vc+2,'b',1,15,0,1)
end

COR_end
