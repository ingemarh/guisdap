% GUISDAP v8.5   07-04-10
% 
COR_init(2000,40)
N_SCAN=length(vc_ch);
for SCAN=0:6:N_SCAN-1;
  % The long pulses
  for vc=(1:2)+SCAN
    COR_lp(1664,12,vc,'s',6,0,15,(0:11)*10,1)
    COR_lp(1844,12,vc,'b',6,0, 4,(0:11)*10,1)
    COR_lp(1892,12,vc,'c',6,0, 1,0,1)
  end

  % E-region power profile
  for vc=(3:4)+SCAN
    COR_pp(1079,1,vc,'s',1,220,12,2)
  end

  % The alternating codes
  for vc=(5:6)+SCAN
    COR_alter(0:6,7,vc,'s',1,120,8,42,(1:7)*42,12,2)
    COR_pp( 861,1,vc,'x',1,218,12,2)
    COR_pp(1299,1,vc,'b',1,315,12,2)
    COR_pp(1614,1,vc,'c',1, 50,12,2)
  end
end
COR_end
