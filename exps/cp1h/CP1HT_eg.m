%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.1       Sodankyla  17 Aug 1990                       %
%   copyright Markku Lehtinen, Asko Huuskonen, Matti Vallinkoski    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function defines the experiment grid for the CP1H experiment
% The grid points are taken from
%  -  the multipulse modulation ranges (first 72)
%  -  the 30us power profile pulse ranges (up to 300 km)
%  -  the long pulse ranges
% function eg_r=CP1HT_eg
function eg_r=CP1HT_eg

len=0;

lpg=3;  % 14us power profile;
points=1:8;
eg_r(len+points-points(1)+1)=lpg_h(lpg)+(points-1)*lpg_dt(lpg);
len=len+length(points);

lpg=28; % zero lag of multipulse ACF's, from the poser profile
points=1:lpg_nt(lpg);
eg_r(len+points-points(1)+1)=lpg_h(lpg)+(points-1)*lpg_dt(lpg);
len=len+length(points);

lpg=42; % 30us power profile;
points=1:lpg_nt(lpg);
heights=lpg_h(lpg)+(points-1)*lpg_dt(lpg);
points=find(heights>eg_r(len)+lpg_dt(28) & heights<2000);
eg_r(len+points-points(1)+1)=lpg_h(lpg)+(points-1)*lpg_dt(lpg);
len=len+length(points);

lpg=95; % long pulse zero lag;
points=1:lpg_nt(lpg);
heights=lpg_h(lpg)+(points-1)*lpg_dt(lpg);
points=find(heights>eg_r(len)+lpg_dt(42));
eg_r(len+points-points(1)+1)=lpg_h(lpg)+(points-1)*lpg_dt(lpg);
len=len+length(points);

eg_r=floor(eg_r);   % make sure that ranges are integer valued
