%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.52       Helsinki 6 March 1994    %
%   copyright  Asko Huuskonen, Markku Lehtinen   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ionc3T_LP.m

COR_init

for vc=1:64
COR_alter(0:14,15,vc,'s',1,101,16,22,[1:15]*22,0,1);
COR_0alter(2012,1,vc,'s',1,102,16,22,0,1);
COR_pp(2145,1,vc, 'x', 1, 132, 0, 1);
COR_pp(1981,1,vc, 'b', 15, 2, 0, 1);
COR_pp(1983,1,vc, 'c', 15, 2, 0, 1);
end
for vc=65:128
COR_lp(1515,16,vc, 's', 10, 0,28, 0:14:210, 2);
COR_lp(1963,16,vc, 'b', 30, 0,1 , 0:14:210, 2);
COR_pp(1979,1,vc, 'c', 10, 2, 0, 2);
end


COR_end
