function [ XXX,YYY,ZZZ, CT,ST,CP,SP ] = GEODETIC2CARTESIAN( GLAT, GLON, ALT )
%GEODETIC2CARTESIAN converts geodetic latitude, longitude and altitude into cartesian coordinates
  RLAT=double(GLAT)*IGRF.UMR;
  CT=sin(RLAT);
  ST=cos(RLAT);
  D=sqrt(IGRF.AQUAD-(IGRF.AQUAD-IGRF.BQUAD)*CT*CT);
  RLON=double(GLON)*IGRF.UMR;
  CP=cos(RLON);
  SP=sin(RLON);
  ZZZ=(double(ALT)+IGRF.BQUAD/D)*CT;
  RHO=(double(ALT)+IGRF.AQUAD/D)*ST;
  XXX=RHO*CP;
  YYY=RHO*SP;
end

