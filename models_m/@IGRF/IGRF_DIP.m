function [ DEC,DIP,DIPL,YMODIP,context ] = IGRF_DIP( context, xlat,xlong,YEAR,height )
%IGRF_DIP magnetic dip calculator
%      subroutine IGRF_DIP(xlat,xlong,year,height,dec,dip,dipl,ymodip)
%-----------------------------------------------------------------------        
% INPUT:
%    xlat      geodatic latitude in degrees
%    xlong     geodatic longitude in degrees
%    year      decimal year (year+month/12.0-0.5 or 
%                  year+day-of-year/365 or ../366 if leap year) 
%    height    height in km
% OUTPUT:
%    dec       magnetic declination in degrees
%    dip       magnetic inclination (dip) in degrees
%    dipl      dip latitude in degrees
%    ymodip    modified dip latitude = asin{dip/sqrt[dip^2+cos(LATI)]} 
%-----------------------------------------------------------------------        

%      COMMON/IGRF1/     UMR,ERA,AQUAD,BQUAD
%
  %context = INITIZE(context);
  %
  %----------------CALCULATE PROFILES-----------------------------------
  %
  XLATI = xlat;
  XLONGI = xlong;
  H = height;
  [~,context] = context.FELDCOF(YEAR);
  [BNORTH,BEAST,BDOWN,BABS] = context.FELDG(XLATI,XLONGI,H);
  DEC=asin(BEAST/sqrt(BEAST*BEAST+BNORTH*BNORTH));
  DIP=asin(BDOWN/BABS);
  dipdiv=DIP/sqrt(DIP*DIP+cos(XLATI*IGRF.UMR));
  if abs(dipdiv) > 1.
    dipdiv=sign(dipdiv);
  end
  SMODIP=asin(dipdiv);
        
  %       DIPL1=atan(0.5*tan(DIP))/IGRF.UMR;

  DIPL=atan(BDOWN/2.0/sqrt(BNORTH*BNORTH+BEAST*BEAST))/IGRF.UMR;
  YMODIP=SMODIP/IGRF.UMR;
  DEC=DEC/IGRF.UMR;
  DIP=DIP/IGRF.UMR;

end

