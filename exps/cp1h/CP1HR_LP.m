%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 8.40               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

global vc_adcint vc_ch
COR_init
for vc=1:length(vc_adcint)
 COR_trilp(0,30,vc,'s',30, -10,5,(0:29)*10,1)
 COR_box(150,30,vc,'b',300, 0,2,(0:29)*10,1)
 COR_box(210,30,vc,'c',300, 0,1,0,1)
end
COR_end
