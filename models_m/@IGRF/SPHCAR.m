function [ xout, yout, zout ] = SPHCAR( xin,yin,zin,J )
%SPHCAR CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
%      SUBROUTINE SPHCAR(R,TETA,PHI,X,Y,Z,J)
%  *********************************************************************
%   CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
%    (TETA AND PHI IN RADIANS).
%                  J>0            J<0
%-----INPUT:   J,R,TETA,PHI     J,X,Y,Z
%----OUTPUT:      X,Y,Z        R,TETA,PHI
%  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
%      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
%      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)
%  *********************************************************************

%        IMPLICIT NONE
%
%        REAL R,TETA,PHI,X,Y,Z,SQ
%
%        INTEGER J

  if J == IGRF.CARTESIAN_INPUT
    X = xin;
    Y = yin;
    Z = zin;
    SQ=X^2+Y^2;
    R=sqrt(SQ+Z^2);
    if SQ == 0.
      PHI=0.;
      if Z >= 0.
        TETA=0.;
      else
        TETA=IGRF.pi;
      end
    else
      SQ=sqrt(SQ);
      PHI=atan2(Y,X);
      TETA=atan2(SQ,Z);
      if PHI < 0.
        PHI=PHI+IGRF.twopi;
      end
    end
    xout = R;
    yout = TETA;
    zout = PHI;
  else
    R = xin;
    TETA = yin;
    PHI = zin;
    SQ=R*sin(TETA);
    X=SQ*cos(PHI);
    Y=SQ*sin(PHI);
    Z=R*cos(TETA);
    xout = X;
    yout = Y;
    zout = Z;
  end

end

