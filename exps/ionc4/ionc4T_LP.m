%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.52       Helsinki 6 March 1994    %
%   copyright  Asko Huuskonen, Markku Lehtinen   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ionc3T_LP.m

COR_init

for vc=1:96
COR_alter(0:14,15,vc,'s',1,117,16,22,[1:15]*22,0,1);
COR_0alter(1787,1,vc,'s',1,118,16,22,0,1);
COR_pp(1933,1,vc, 'x', 1, 148, 0, 1);
COR_pp(1755,1,vc, 'b', 15, 2, 0, 1);
COR_pp(1757,1,vc, 'c', 15, 2, 0, 1);
end


COR_end
