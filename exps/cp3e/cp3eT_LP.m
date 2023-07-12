% GUISDAP v1.50   94-03-10 Copyright Asko Huuskonen, Markku Lehtinen 
% 
% CP3ET_LP.m
COR_init

for vc=1:8,
  COR_pp(  0,1,vc,'s',1,180,0,1)
  COR_pp(180,1,vc,'b',1, 24,0,1)
  COR_pp(204,1,vc,'c',1,  6,0,1)
end

for vc=9:12,
  COR_lp( 210,16,vc,'s',15, 0,25,0:14:210,2)
  COR_lp( 610,16,vc,'b',15, 0, 7,0:14:210,2)
  COR_lp( 722,16,vc,'c',15, 0, 2,0       ,2)
end

COR_end
