function [ SX,GRO ] = ROGUL( IDAY,XHI )
%ROGUL CALCULATES RATIO H0.5/HMF2 FOR HALF-DENSITY POINT
%
%        SUBROUTINE ROGUL(IDAY,XHI,SX,GRO)
% --------------------------------------------------------------------- 
%   CALCULATES RATIO H0.5/HMF2 FOR HALF-DENSITY POINT (NE(H0.5)=0.5*
%   NMF2) T. GULYAEVA, ADVANCES IN SPACE RESEARCH 7, #6, 39-48, 1987.
%
%       INPUT:  IDAY    DAY OF YEAR
%               XHI     SOLAR ZENITH ANGLE [DEGREE]
%       
%       OUTPUT: GRO     RATIO OF HALF DENSITY HEIGHT TO F PEAK HEIGHT
%               SX      SMOOTHLY VARYING SEASON PARAMTER (SX=1 FOR 
%                       DAY=1; SX=3 FOR DAY=180; SX=2 FOR EQUINOX)
% ---------------------------------------------------------------------

%        common  /const1/humr,dumr
  SX = 2. - cos ( IDAY * IRI2012.dumr );
  XS = ( XHI - 20. * SX) / 15.;
  GRO = 0.8 - 0.2 / ( 1. + exp(XS) );
  % same as gro=0.6+0.2/(1+exp(-xs))

end

