function [ RPID ] = RPID( H, H0, N0, M, ST, ID, XS )
%RPID RELATIVE PRECENTAGE DENSITY OF ATOMIC AND MOLECULAR OXYGEN IONS
%
%                     
%*************************************************************                  
%************* ION RELATIVE PRECENTAGE DENSITY *****************                
%*************************************************************                  
%
%
%      REAL FUNCTION RPID (H, H0, N0, M, ST, ID, XS)
%------------------------------------------------------------------
% D.BILITZA,1977,THIS ANALYTIC FUNCTION IS USED TO REPRESENT THE                
% RELATIVE PRECENTAGE DENSITY OF ATOMAR AND MOLECULAR OXYGEN IONS.              
% THE M+1 HEIGHT GRADIENTS ST(M+1) ARE CONNECTED WITH EPSTEIN-                  
% STEP-FUNCTIONS AT THE STEP HEIGHTS XS(M) WITH TRANSITION                      
% THICKNESSES ID(M). RPID(H0,H0,N0,....)=N0.       
% ARGMAX is the highest allowed argument for EXP in your system.
%------------------------------------------------------------------

%      REAL              N0         
%      DIMENSION         ID(4), ST(5), XS(4)                
%      COMMON  /ARGEXP/  ARGMAX

  SUM=(H-H0)*ST(1);
  for I=1:M
    XI=ID(I);
    AA = IRI2012.EPTR(H ,XI,XS(I));
    BB = IRI2012.EPTR(H0,XI,XS(I));
    SUM=SUM+(ST(I+1)-ST(I))*(AA-BB)*XI;
  end
  if abs(SUM) < IRI2012.ARGMAX
    SM=exp(SUM);
  elseif SUM > 0.0
    SM=exp(IRI2012.ARGMAX);
  else
    SM=0.0;
  end
  RPID = N0 * SM;

end

