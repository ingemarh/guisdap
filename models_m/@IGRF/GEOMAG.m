function [ xout,yout,zout ] = GEOMAG( context, xin,yin,zin,J )
%GEOMAG CONVERTS GEOCENTRIC (GEO) TO DIPOLE (MAG) COORDINATES OR VICA VERSA
%      SUBROUTINE GEOMAG(XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,J,IYR)
%  *********************************************************************
% CONVERTS GEOCENTRIC (GEO) TO DIPOLE (MAG) COORDINATES OR VICA VERSA.
% IYR IS YEAR NUMBER (FOUR DIGITS).
%
%                           J=0                J=1
%-----INPUT:  J,XGEO,YGEO,ZGEO,IYR   J,XMAG,YMAG,ZMAG,IYR
%-----OUTPUT:    XMAG,YMAG,ZMAG        XGEO,YGEO,ZGEO
%
%  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
%      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
%      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
%  *********************************************************************

%        IMPLICIT NONE
%
%        REAL XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,ST0,CT0,SL0,CL0,CTCL,
%     *       STCL,CTSL,STSL,AB(19),BB(8)
%
%        INTEGER J,IYR,K,IY,II
%
%      COMMON/C1/ ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,AB,K,IY,BB
  if J == IGRF.GEOGRAPHIC_COORDINATES
    XGEO = xin;
    YGEO = yin;
    ZGEO = zin;
    XMAG=XGEO*context.CTCL+YGEO*context.CTSL-ZGEO*context.ST0;
    YMAG=YGEO*context.CL0-XGEO*context.SL0;
    ZMAG=XGEO*context.STCL+YGEO*context.STSL+ZGEO*context.CT0;
    xout = XMAG;
    yout = YMAG;
    zout = ZMAG;
  else
    XMAG = xin;
    YMAG = yin;
    ZMAG = zin;
    XGEO=XMAG*context.CTCL-YMAG*context.SL0+ZMAG*context.STCL;
    YGEO=XMAG*context.CTSL+YMAG*context.CL0+ZMAG*context.STSL;
    ZGEO=ZMAG*context.CT0-XMAG*context.ST0;
    xout = XGEO;
    yout = YGEO;
    zout = ZGEO;
  end

end

