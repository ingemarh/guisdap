function [ XL,ICODE,DIPL,BABS,context ] = IGRF_SUB( context, xlat,xlong,YEAR,HEIGHT )
%IGRF_SUB Subroutines to compute IGRF parameters for IRI and all functions
% igrf.for, version number can be found at the end of this comment.
%-----------------------------------------------------------------------        
%
% Subroutines to compute IGRF parameters for IRI and all functions and 
% subroutines required for this computation, including:
% 	IGRF_SUB, IGRF_DIP, FINDB0, SHELLG, STOER, FELDG, FELDCOF, GETSHC, 
% 	INTERSHC, EXTRASHC, INITIZE, GEODIP, fmodip
%
% CGM coordinates : GEOCGM01, OVL_ANG, CGMGLA, CGMGLO, DFR1DR, 
%   AZM_ANG, MLTUT, MFC, FTPRNT, GEOLOW, CORGEO, GEOCOR, SHAG, RIGHT, 
%   IGRF, RECALC, SPHCAR, BSPCAR, GEOMAG, MAGSM, SMGSM
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Required i/o units:  
%  KONSOL= 6 Program messages (used when jf(12)=.true. -> konsol)
%  KONSOL=11 Program messages (used when jf(12)=.false. -> MESSAGES.TXT)
%
%     COMMON/iounit/konsol is used to pass the value of KONSOL from 
%     IRISUB to IRIFUN and IGRF. If KONSOL=1 than messages are turned off.
%     
%  UNIT=14 IGRF/GETSHC: IGRF coeff. (DGRF%%%%.DAT or IGRF%%%%.DAT, %%%%=year)
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Corrections:
% 11/01/91 SHELLG: lowest starting point for B0 search is 2  
%  1/27/92 Adopted to IGRF-91 coeffcients model
%  2/05/92 Reduce variable names: INTER(P)SHC,EXTRA(P)SHC,INITI(ALI)ZE
%  8/08/95 Updated to IGRF-45-95; new coeff. DGRF90, IGRF95, IGRF95S
%  5/31/00 Updated to IGRF-45-00; new coeff.: IGRF00, IGRF00s
%-Version-mm/dd/yy-Description (Person reporting the correction)
% 2000.01 05/07/01 initial version
% 2000.02 07/11/01 replace feldi(xi,h) by feldi (P. Wilkinson)
% 2000.02 07/11/01 variables EGNR, AGNR,OGNR not used (P. Wilkinson)
% 2000.01 10/28/02 replace TAB/6 blanks, enforce 72/line (D. Simpson)
% 2000.02 11/08/02 change unit for coefficients to 14
% 2000.03 06/05/03 correct DIPL computation (V. Truhlik)
% 2005.00 04/25/05 CALL FELDI and DO 1111 I=1,7 (Alexey Petrov)
% 2005.01 11/10/05 added igrf_dip and geodip (MLAT) 
% 2005.02 11/10/05 FELDCOF: updated to IGRF-10 version
% 2005.03 12/21/06 GH2(120) -> GH2(144)
% 2007.00 05/18/07 Release of IRI-2007
% 2007.08 07/30/09 SHELLG,STOER,FELDG,FELDCOF: NMAX=13; H/G-arrays(195) 
% 2007.10 02/26/10 FELDCOF: updated to IGRF-11; DGRF05, IGRF10, IGRF10S
% 2007.11 04/27/10 RECALC: updated to IGRF-11
% 2007.11 04/27/10 Make all arrays(195) to arrays(196) 
% 2007.11 04/27/10 FELDCOF: corrected Filmod and also IGRF10.DAT
% 2007.11 04/29/10 New files dgrf%%%%.asc; new GETSHC; char*12 to 13
%
% 2012.00 10/05/11 IRI-2012: bottomside B0 B1 model (SHAMDB0D, SHAB1D),
% 2012.00 10/05/11    bottomside Ni model (iriflip.for), auroral foE
% 2012.00 10/05/11    storm model (storme_ap), Te with PF10.7 (elteik),
% 2012.00 10/05/11    oval kp model (auroral_boundary), IGRF-11(igrf.for), 
% 2012.00 10/05/11    NRLMSIS00 (cira.for), CGM coordinates, F10.7 daily
% 2012.00 10/05/11    81-day 365-day indices (apf107.dat), ap->kp (ckp),
% 2012.00 10/05/11    array size change jf(50) outf(20,1000), oarr(100).
% 2012.02 12/17/12 igrf_dip: Add magnetic declination as output parameter
%-----------------------------------------------------------------------        
%
%        subroutine IGRF_SUB(xlat,xlong,year,height,
%                xl,icode,dipl,babs)
%-----------------------------------------------------------------------        
% INPUT:
%    xlat      geodatic latitude in degrees
%    xlong     geodatic longitude in degrees
%    year      decimal year (year+month/12.0-0.5 or 
%                  year+day-of-year/365 or ../366 if leap year) 
%    height    height in km
% OUTPUT:
%    xl        L value
%    icode      =1  L is correct; =2  L is not correct;
%               =3  an approximation is used
%    dipl      dip latitude in degrees
%    babs      magnetic field strength in Gauss
%-----------------------------------------------------------------------        

%      REAL              LATI,LONGI
%      COMMON/IGRF1/     UMR,ERA,AQUAD,BQUAD
%
  LATI=xlat;
  LONGI=xlong;
  %
  %----------------CALCULATE PROFILES-----------------------------------
  %
  [DIMO,context] = context.FELDCOF(YEAR);
  [BNORTH,BEAST,BDOWN,BABS] = context.FELDG(LATI,LONGI,HEIGHT);
  [XL,ICODE,~] = context.SHELLG(LATI,LONGI,HEIGHT,DIMO);
  %DIP=asin(BDOWN/BABS)/IGRF.UMR
  %DEC=asin(BEAST/sqrt(BEAST*BEAST+BNORTH*BNORTH))/IGRF.UMR
  %DIPL=atan(0.5*tan(DIP*UMR))/IGRF.UMR
  DIPL=atan(BDOWN/2.0/sqrt(BNORTH*BNORTH+BEAST*BEAST))/IGRF.UMR;

end

