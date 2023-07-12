%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.52       Helsinki 6 March 1994    %
%   copyright  Asko Huuskonen, Markku Lehtinen   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ionc3T_LP.m

COR_init

for vc=1:96
COR_alter(0:14,15,vc,'s',1,117,16,22,[1:15]*22,0,1);
%COR_0alter(1787,1,vc,'s',1,118,16,22,0,1);
COR_pp(6432,1,vc,'x', 1,2,0,1);
COR_pp(1755,1,vc,'b',15,2,0,1);
COR_pp(1757,1,vc,'c',15,2,0,1);
end
for i=0:31
 for vc=97+3*i:3*i+99
  COR_pp(1759+i*146,1,vc,'s',1,146,0,1);
 end
end


COR_end
