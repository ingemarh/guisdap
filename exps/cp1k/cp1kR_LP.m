%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.1       Sodankyla  17 Aug 1990                       %
%   copyright Markku Lehtinen, Asko Huuskonen, Matti Vallinkoski    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To save memory and computation time, only half of the virtual
% channels may have been defined. For a full defintion, the length of
% vc_ch is 256, and half of that for the economy definition
N_SCAN=length(vc_ch)/4;

% STEP=1;
% The transmission is always repeated on even and odd channels
% It is possible to reduce the number of lag profiles by dropping
% either one. This happens, if  STEP=2

STEP=2;
% The p_ND variable takes care of the effects where needed
p_ND=STEP*64/N_SCAN;
                                                                     
COR_init(10000/p_ND,320)
for scan=0:N_scan-1;
  % The long pulses
  for vc=(1:STEP:2)+scan*4
    COR_trilp( 60,30,vc,'s', 32, 0, 1,0:10:290,3)
      COR_box(150,30,vc,'b',320, 0, 2,0:10:290,3)
      COR_box(210,30,vc,'c',320, 0, 1,0,3) 
  end
   
% The alternating codes
  for vc=(3:STEP:4)+scan*4
    ra=240+(0:14)+3*15;
    COR_alter( ra,15,vc,'s',1,1,16,21,(1:15)*21,0,1);
      COR_box(345,16,vc,'b',80, 0, 2,0,1)
      COR_box(377,16,vc,'c',80, 0, 1,0,1) 
  end
end
   
COR_end
