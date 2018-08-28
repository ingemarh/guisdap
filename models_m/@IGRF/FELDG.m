function [ BNORTH,BEAST,BDOWN,BABS ] = FELDG( context, GLAT,GLON,ALT )
%FELDG CALCULATES EARTH MAGNETIC FIELD FROM SPHERICAL HARMONICS MODEL
%      SUBROUTINE FELDG(GLAT,GLON,ALT,BNORTH,BEAST,BDOWN,BABS)           
%-----------------------------------------------------------------------        
% CALCULATES EARTH MAGNETIC FIELD FROM SPHERICAL HARMONICS MODEL
% REF: G. KLUGE, EUROPEAN SPACE OPERATIONS CENTRE, INTERNAL NOTE 61, 
%      1970.
%-----------------------------------------------------------------------        
% CHANGES (D. BILITZA, NOV 87):
%   - FIELD COEFFICIENTS IN BINARY DATA FILES INSTEAD OF BLOCK DATA
%   - CALCULATES DIPOL MOMENT
% 09/07/22 NMAX=13 for DGRF00 and IGRF05; H/G-arrays(195)
%-----------------------------------------------------------------------        
%  INPUT:  ENTRY POINT FELDG
%               GLAT  GEODETIC LATITUDE IN DEGREES (NORTH)
%               GLON  GEODETIC LONGITUDE IN DEGREES (EAST)
%               ALT   ALTITUDE IN KM ABOVE SEA LEVEL
%
%          ENTRY POINT FELDC
%               V(3)  CARTESIAN COORDINATES IN EARTH RADII (6371.2 KM)
%                       X-AXIS POINTING TO EQUATOR AT 0 LONGITUDE
%                       Y-AXIS POINTING TO EQUATOR AT 90 LONG.
%                       Z-AXIS POINTING TO NORTH POLE
%
%          COMMON BLANK AND ENTRY POINT FELDI ARE NEEDED WHEN USED
%            IN CONNECTION WITH L-CALCULATION PROGRAM SHELLG.
%       
%          COMMON /MODEL/ AND /IGRF1/
%               UMR     = ATAN(1.0)*4./180.   <DEGREE>*UMR=<RADIANT>
%               ERA     EARTH RADIUS FOR NORMALIZATION OF CARTESIAN 
%                       COORDINATES (6371.2 KM)
%               AQUAD, BQUAD   SQUARE OF MAJOR AND MINOR HALF AXIS OF 
%                       EARTH ELLIPSOID AS RECOMMENDED BY INTERNAT. 
%                       ASTRONOMICAL UNION (6378.160, 6356.775 KM).
%               NMAX    MAXIMUM ORDER OF SPHERICAL HARMONICS
%               TIME    YEAR (DECIMAL: 1973.5) FOR WHICH MAGNETIC 
%                       FIELD IS TO BE CALCULATED
%               G(M)    NORMALIZED FIELD COEFFICIENTS (SEE FELDCOF)
%                       M=NMAX*(NMAX+2)
%-----------------------------------------------------------------------        
%  OUTPUT: BABS   MAGNETIC FIELD STRENGTH IN GAUSS
%          BNORTH, BEAST, BDOWN   COMPONENTS OF THE FIELD WITH RESPECT
%                 TO THE LOCAL GEODETIC COORDINATE SYSTEM, WITH AXIS
%                 POINTING IN THE TANGENTIAL PLANE TO THE NORTH, EAST
%                 AND DOWNWARD.   
%-----------------------------------------------------------------------

%      DIMENSION         V(3),B(3)   
%      CHARACTER*13      NAME
%      COMMON/IGRF2/	XI(3),H(196)
%      COMMON/MODEL/     NMAX,TIME,G(196),NAME  
%      COMMON/IGRF1/     UMR,ERA,AQUAD,BQUAD

%
%-- IS RECORDS ENTRY POINT
%
  %*****ENTRY POINT  FELDG  TO BE USED WITH GEODETIC CO-ORDINATES         
  IS=1;
  [ XXX,YYY,ZZZ, CT,ST,CP,SP ] = IGRF.GEODETIC2CARTESIAN( GLAT, GLON, ALT );
  XXX = XXX / context.ERA;
  YYY = YYY / context.ERA;
  ZZZ = ZZZ / context.ERA;
  %      GOTO 10                                                            

  %*****ENTRY POINT  FELDC  TO BE USED WITH CARTESIAN CO-ORDINATES        
  %      ENTRY FELDC(V,B)                                                  
  %      IS=2;                                                              
  %      XXX=V(1);                                                          
  %      YYY=V(2);                                                          
  %      ZZZ=V(3);                                                          
  RQ=1./(XXX*XXX+YYY*YYY+ZZZ*ZZZ); 
  [~,BABS,BEAST,BNORTH,BDOWN] = context.FELDI(RQ, XXX, YYY, ZZZ, IS, ...
    CP, SP, CT, ST);


end

