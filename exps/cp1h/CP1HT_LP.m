%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.1       Sodankyla  17 Aug 1990                       %
%   copyright Markku Lehtinen, Asko Huuskonen, Matti Vallinkoski    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
COR_init

for vc=9:12,
  COR_pp( 0,1,vc,'b',3,15,0,1)
  COR_pp(15,1,vc,'c',3, 5,0,1)
  COR_pp(20,1,vc,'s',3,40,0,1)
end

for vc=5:8,
  COR_mp( 60,12,vc,'b',3,25,0:30:330,0,2)
  COR_mp(360,12,vc,'c',3, 5,0:30:330,0,2)
end

for vc=9:12,
  COR_pp(420,12,vc,'s',3,60,24,2)
end

for vc=5:8,
  COR_mp(421,12,vc,'s',3,60,30:30:270,0,2)
  COR_mp(430,12,vc,'o',3,60,   300   ,0,2)
  COR_mp(431,12,vc,'s',3,60,   330   ,0,2)
end

for vc=1:2
  COR_pp(1140,1,vc,'b',3,15,0,3)
  COR_pp(1155,1,vc,'c',3, 5,0,3)
  COR_pp(1160,1,vc,'s',3,81,0,3)
end

for vc=3:4,
  COR_lp(1241,26,vc,'b',15,50, 5,0:10:250,4)
  COR_lp(1371,26,vc,'c',15, 0, 1,0:10:250,4)
  COR_lp(1397,26,vc,'s',15, 0,21,0:10:250,4)
end

COR_end
