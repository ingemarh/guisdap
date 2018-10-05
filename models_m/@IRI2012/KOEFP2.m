function [ PG2O ] = KOEFP2(  )
%KOEFP2 COEFFICIENTS FOR CALCULATION OF O+ PROFILES
%
%      SUBROUTINE KOEFP2(PG2O)                      
% THIEMANN,1979,COEFFICIENTS FOR CALCULATION OF O+ PROFILES                     
% ABOVE THE F2-MAXIMUM (DUMBS,SPENNER:AEROS-COMPILATION)                        

%      DIMENSION PG2O(32)                           
%      REAL FELD(32)
  persistent FELD;
  if isempty(FELD)
    FELD = [1.0,-11.0,-11.0,1.0,695.0,-.000781, ...
      -.00264,2177.0,1.0,-11.0,-11.0,2.0,570.0, ...
      -.002,-.0052,1040.0,2.0,-11.0,-11.0,1.0,695.0, ...
      -.000786,-.00165,3367.0,2.0,-11.0,-11.0,2.0, ...
      575.0,-.00126,-.00524,1380.0];
  end
  PG2O = FELD;

end

