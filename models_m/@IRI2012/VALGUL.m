function [ HVB,VWU,VWA,VDP ] = VALGUL( XHI )
%VALGUL CALCULATES E-F VALLEY PARAMETERS
%
%        SUBROUTINE VALGUL(XHI,HVB,VWU,VWA,VDP)
% --------------------------------------------------------------------- 
%   CALCULATES E-F VALLEY PARAMETERS; T.L. GULYAEVA, ADVANCES IN
%   SPACE RESEARCH 7, #6, 39-48, 1987.
%
%       INPUT:  XHI     SOLAR ZENITH ANGLE [DEGREE]
%       
%       OUTPUT: VDP     VALLEY DEPTH  (NVB/NME)
%               VWU     VALLEY WIDTH  [KM]
%               VWA     VALLEY WIDTH  (SMALLER, CORRECTED BY RAWER)
%               HVB     HEIGHT OF VALLEY BASE [KM]
% -----------------------------------------------------------------------

%        COMMON  /CONST/UMR
%
  CS = 0.1 + cos(IRI2012.UMR*XHI);
  ABC = abs(CS);
  VDP = 0.45 * CS / (0.1 + ABC ) + 0.55;
  ARL = ( 0.1 + ABC + CS ) / ( 0.1 + ABC - CS);
  ZZZ = log( ARL );
  VWU = 45. - 10. * ZZZ;
  VWA = 45. -  5. * ZZZ;
  HVB = 1000. / ( 7.024 + 0.224 * CS + 0.966 * ABC );

end

