function [ FOEEDI ] = FOEEDI( COV,XHI,XHIM,XLATI )
%FOEEDI CALCULATES FOE/MHZ
%
%        REAL FUNCTION FOEEDI(COV,XHI,XHIM,XLATI)
%-------------------------------------------------------
% CALCULATES FOE/MHZ BY THE EDINBURGH-METHOD.      
% INPUT: MONTHLY MEAN 10.7CM SOLAR RADIO FLUX measured at ground level  
% (COV), GEOGRAPHIC LATITUDE (XLATI/DEG), SOLAR ZENITH ANGLE (XHI/DEG 
% AND XHIM/DEG AT NOON).
% REFERENCE: 
%       KOURIS-MUGGELETON, CCIR DOC. 6/3/07, 1973
%       TROST, J. GEOPHYS. RES. 84, 2736, 1979 (was used
%               to improve the nighttime varition)
%       RAWER AND BILITZA, Adv. Space Res. 10(8), 5-14, 1990
% D.BILITZA--------------------------------- AUGUST 1986.    

%        COMMON/CONST/UMR
  % variation with solar activity (factor A) ...............
  A=1.0+0.0094*(COV-66.0);
  % variation with noon solar zenith angle (B) and with latitude (C)
  SL=cos(XLATI*IRI2012.UMR);
  if XLATI < 32.0
    SM=-1.93+1.92*SL;
    C=23.0+116.0*SL;
  else
    SM=0.11-0.49*SL;
    C=92.0+35.0*SL;
  end
  if XHIM >= 90.
    XHIM=89.999;
  end
  B = cos(XHIM*IRI2012.UMR) ^ SM;
  % variation with solar zenith angle (D) ..........................        
  if XLATI > 12.0
    SP=1.2;
  else
    SP=1.31;
  end
  % adjusted solar zenith angle during nighttime (XHIC) .............
  XHIC=XHI-3.*log(1.+exp((XHI-89.98)/3.));
  D=cos(XHIC*IRI2012.UMR)^SP;
  % determine foE**4 ................................................
  R4FOE=A*B*C*D;
  % minimum allowable foE (foe_min=sqrt[SMIN])...............................
  SMIN=0.121+0.0015*(COV-60.);
  SMIN=SMIN*SMIN;
  if R4FOE < SMIN
    R4FOE=SMIN;
  end
  FOEEDI=R4FOE^0.25;

end

