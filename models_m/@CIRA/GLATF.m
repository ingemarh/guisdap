function [ GV, REFF, GVdLat, REFFdLat ] = GLATF( context, LAT )
%GLATF CALCULATE LATITUDE VARIABLE GRAVITY AND EFFECTIVE RADIUS
%      SUBROUTINE GLATF(LAT,GV,REFF)
%-----------------------------------------------------------------------
%      CALCULATE LATITUDE VARIABLE GRAVITY (GV) AND EFFECTIVE
%      RADIUS (REFF)
%      LAT geodetic latitude (degrees)
%      GV latitude variable gravity (cm/s^2)
%      REFF effective radius (km)
%-----------------------------------------------------------------------

%      REAL LAT
%      SAVE
  % gravity at surface is G=GV/(1.+ALT/REFF)^2

  C2 = cos(2.*CIRA.DGTR*double(LAT));
  gC2 = polyval(context.g,C2); % cm/s^2
  pC2 = polyval(context.p,C2);
  GV = gC2;
  REFF = 2.*gC2/pC2; % km

  C2dLat = -sin(2.*CIRA.DGTR*double(LAT))*2.*CIRA.DGTR;
  dg = polyval(polyder(context.g),C2)*C2dLat;
  dp = polyval(polyder(context.p),C2)*C2dLat;
  GVdLat = dg;
  REFFdLat = (2.*dg - REFF*dp)/pC2;
  %meanRadius = 6371.009;
  
  % 6377.8199903009 km at poles, WGS84 says 6356.7523142452
  % 6334.9398291251 km at equator, WGS84 says 6378.137
  % 6356.3641505 km at 45deg Lat, WGS84 says 6367.4895438635
  % 983.2021785768 cm/s^2 at poles, WGS84 says 983.21849378
  % 978.0298214232 cm/s^2 at equator, WGS84 says 978.03253359
  % 980.616 cm/s^2 at 45deg Lat, WGS84 says 980.6197769344
%   REQ = 6378.137;
%   FI = 298.257223563;
%   GPL = 983.21849378;
%   GEQ = 978.03253359;
%   
%   E2 = (2.0 - 1.0 / FI)/ FI;
%   
%   A =  (GPL + GEQ - GPL / FI)*0.5/sqrt(1.0-E2*0.5);
%   B = -(1 - 1 / FI - GEQ/GPL)/(1 - 1 / FI + GEQ/GPL);
%   C = E2/(2.0-E2);
%   AR = REQ*sqrt((1.0-(2.0-E2)*E2*0.5)/(1.0-E2*0.5));
%   CR = E2*0.5/(1/(2.0-E2)-E2*0.5);
%   
%   GV = A*(1+B*C2)/sqrt(1.0+C*C2);
%   REFF = AR*sqrt((1+CR*C2)/(1.0+C*C2));
%   GVdLat = GV*(B*C2dLat)/(1+B*C2)-0.5*GV*C*C2dLat/(1.0+C*C2);
%   REFFdLat = 0.5*REFF*CR*C2dLat/(1+CR*C2)-0.5*REFF*C*C2dLat/(1.0+C*C2);
% 
%   GV = 980.61977693*(1-9.65E-04*C2)/sqrt(1.0+3.358E-03*C2);
%   REFF = 6367.4895439*sqrt((1+6.717E-03*C2)/(1.0+3.358E-03*C2));
  
end

