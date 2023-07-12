% GUISDAP v1.50   94-03-10 Copyright Asko Huuskonen, Markku Lehtinen 
% 
                                                                     
COR_init(10000,40)
% To save memory and computation time, only half of the virtual
% channels may have been defined. For a full defintion, the length of
% vc_ch is 640, and half of that for the economy definition
N_SCAN=length(vc_ch)/10;
% STEP=1;
% The transmission is always repeated on even and odd channels
% It is possible to reduce the number of lag profiles by dropping
% either one. This happens, if  STEP=2

STEP=2;
% The p_ND variable takes care of the effects where needed
p_ND=STEP*64/N_SCAN;

for SCAN=0:N_SCAN-1;
  % The low resolution power profile
  for vc=(1:STEP:2)+SCAN*10
    COR_pp(1181,1,vc,'s',3,80,0,2)
    COR_pp(1261,1,vc,'b',3,40,0,2)
    COR_pp(1301,1,vc,'c',3, 6,0,2)
  end
   
  % The long pulses
  for vc=(3:STEP:4)+SCAN*10
    COR_lp(1307,26,vc,'s',15, 0,21,0:10:250,3)
    COR_lp(1853,26,vc,'b',15, 0, 6,0:10:250,3)
		  % The non-zero lags, which actually are not calculated but included
	  	% as zeroes in the data dumps for compatibility with the analysis
				% are omitted here because GUISDAP does not need those
    COR_lp(2009,26,vc,'c',15, 0, 1,0,3) 
  end
   
  % The alternating codes
  for vc=(5:STEP:6)+SCAN*10
    COR_alter(236+(0:14),15,vc,'s',1,61,16,21,(1:15)*21,0,1);
    % Zero lag from the alternating code "variance profile"
    COR_pp(160,1, vc,'x', 1,76,0,1)
  end

  % E-region power profile, also used as zero lag for the ACF's
  for vc=(7:STEP:8)+SCAN*10
    COR_pp(  0,1,vc,'s',1,90,0,1)
  end
 
  for vc=(9:STEP:10)+SCAN*10
    COR_pp(  0,1,vc,'s',1,90,0,1)
    COR_pp( 90,1,vc,'b',1,60,0,1)
    COR_pp(150,1,vc,'c',1,10,0,1)
  end
end
   
   
COR_end
