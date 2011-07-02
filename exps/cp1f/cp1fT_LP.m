%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.1       Sodankyla  17 Aug 1990                       %
%   copyright Markku Lehtinen, Asko Huuskonen, Matti Vallinkoski    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

COR_init

vc=1; lags=[2,12,16,10,14,4]*20;
COR_mp(2+   0,8,vc,'s',1,30,lags,0,1)
for i=[0 lags(1:3)/20]
 COR_pp(1+   0,8,vc,'x',1,30,i,1)
end
COR_mp(2+30*8,8,vc,'c',1, 3,lags,0,1)
for i=[0 lags(1:3)/20]
 COR_pp(1+30*8,8,vc,'c',1,3,i,1)
end
COR_mp(2+33*8,8,vc,'b',1, 3,lags,0,1)
for i=[0 lags(1:3)/20]
 COR_pp(1+33*8,8,vc,'b',1,3,i,1)
end

vc=2; lags=[3,9,11,6]*20; % 11 is garbage
COR_mp(2+36*8,8,vc,'s',1,30,lags,0,2)
for i=[0 lags(1:3)/20]
 COR_pp(1+36*8,8,vc,'x',1,30,i,2)
end
COR_mp(2+66*8,8,vc,'c',1, 3,lags,0,2)
for i=[0 lags(1:3)/20]
 COR_pp(1+66*8,8,vc,'c',1,3,i,2)
end
COR_mp(2+69*8,8,vc,'b',1, 3,lags,0,2)
for i=[0 lags(1:3)/20]
 COR_pp(1+69*8,8,vc,'b',1,3,i,2)
end

vc=3;
COR_pp(8*72    ,1,vc,'s',1, 80,0,3)
COR_pp(8*72+ 80,1,vc,'c',1, 10,0,3)
COR_pp(8*72+ 90,1,vc,'b',1, 10,0,3)
vc=4;
COR_pp(8*72+100,1,vc,'s',1,174,0,4)
COR_pp(8*72+274,1,vc,'c',1, 18,0,4)
COR_pp(8*72+292,1,vc,'b',1, 18,0,4)

vc=5;
COR_trilp(1+8*72+310      ,33,vc,'s',36,-18,23,0:10:310,5)
COR_trilp(1+8*72+310+33*24,33,vc,'c',36,-18, 5,0:10:310,5)
COR_trilp(1+8*72+310+33*30,33,vc,'b',36,-18, 5,0:10:310,5)

COR_end
