function [ GV, REFF, GVdLat, REFFdLat ] = GLATFELLIPSOID( context, LAT )
%GLATF CALCULATE LATITUDE VARIABLE GRAVITY AND EFFECTIVE RADIUS BASED ON ELLIPSOID
%      SUBROUTINE GLATF(LAT,GV,REFF)
%-----------------------------------------------------------------------
%      CALCULATE LATITUDE VARIABLE GRAVITY (GV) AND EFFECTIVE
%      RADIUS (REFF)
%      LAT geodetic latitude (degrees)
%      GV latitude variable gravity (cm/s^2)
%      REFF effective radius (km)
%-----------------------------------------------------------------------

  C2 = cos(2.*CIRA.DGTR*double(LAT));
  C2dLat = -sin(2.*CIRA.DGTR*double(LAT))*2.*CIRA.DGTR;
  REQ = context.REQ;
  FI = context.FI;
  GPL = context.GPL;
  GEQ = context.GEQ;
  
  E2 = (2.0 - 1.0 / FI)/ FI;
  
  A =  (GPL + GEQ - GPL / FI)*0.5/sqrt(1.0-E2*0.5);
  B = -(1.0 - 1.0 / FI - GEQ/GPL)/(1.0 - 1.0 / FI + GEQ/GPL);
  C = E2/(2.0-E2);
  AR = REQ*sqrt((1.0-(1.0-E2*0.5)*E2)/(1.0-E2*0.5));
  CR = E2/(1.0/(1.0-E2*0.5)-E2);
  
  GV = A*(1+B*C2)/sqrt(1.0+C*C2);
  REFF = AR*sqrt((1+CR*C2)/(1.0+C*C2));
  GVdLat = GV*(B*C2dLat)/(1+B*C2)-0.5*GV*C*C2dLat/(1.0+C*C2);
  REFFdLat = 0.5*REFF*CR*C2dLat/(1+CR*C2)-0.5*REFF*C*C2dLat/(1.0+C*C2);

%   GV = 980.61977693*(1-9.65E-04*C2)/sqrt(1.0+3.358E-03*C2);
%   REFF = 6367.4895439*sqrt((1+6.717E-03*C2)/(1.0+3.358E-03*C2));
  
end

