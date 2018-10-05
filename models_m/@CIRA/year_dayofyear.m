function [ YD, SECS, STL ] = year_dayofyear( tjd, GLONG )
% This function will compute a year, day of year from Julian Date as well as solar time
% INPUT:
%   tjd Julian Date
%   GLONG geographic longitude (degrees East)
% OUTPUT:
%   YD Year-Day of Year integer for use with GTD7
%   SECS number of seconds in day
%   STL solar time (hours)
% 
  djd = double(tjd)-1721058.5; % days since Jan 0.0, 0000
  T = (floor(djd)-60.025) / 36524.25; % centuries since Feb 29.025, 0000
  Y     = 100*T-(0.75*T-ceil(0.75*T-mod(T,1.0))) / 365.2425;
  k3    = floor(mod(Y,100) / 4000) + 31 + floor(365.2425*mod(Y,1.0));

  year  = floor(Y + floor(k3/336.4625));
  
  % Julian Date of Jan 1.0
  tjd0 = 1.5 + floor((1461*year+6884227.0)/4);
  tjd0 = tjd0 + floor(2-0.75*floor((year-1)/100));
  doy = double(tjd) - tjd0 + 1.0;
  YD = year*CIRA.YRDSHIFT + doy;
  SECS = mod(YD,1.0)*86400.0;
  YD = floor(YD);
  STL = SECS/3600.0 + GLONG/15.0;
end

