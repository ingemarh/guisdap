function [ HT05 ] = TOPH05( COVI,AMLAT,TIME,HMAX,SG )
%TOF05 empirical RAT=(h05top-hmF2)/hmF2 derived from ISIS and IK19
%                     
%************************************************************                   
%***************** PROFILE PARAMETERS ***********************                   
%************************************************************                 
%
%
%	      SUBROUTINE TOPH05(COVI,AMLAT,TIME,HMAX,HT05,SG)
%-------------------------------------------------------------------------------
% Gulyaeva T.L. (2003) Variations in the half-width of the topside ionosphere 
%    according to the observations by space ionosondes ISIS 1,ISIS 2, and IK19.
%    International J. of Geomagnetism and Aeronomy, 4(3), 201-207.
% Gulyaeva T.L., Titheridge J.E. (2006) Advanced specification of electron density 
%    and temperature in the IRI ionosphere-plasmasphere model. 
%    Adv. Space Res. 38(11), 2587-2595, doi:10.1016/j.asr.2005.08.045.
%
%  Implementation of empirical RAT=(h05top-hmF2)/hmF2 derived from ISIS and IK19
%  topside electron density profiles to obtain half peak density topside height
%  h05top  from the Chebishev polinomial coefficients given for 
%  (1) 4 levels of solar activity: Rz= 0,  50, 100, 150 replaced by
%      solar radio flux          covi=60, 106, 152, 198
%  (2) 10 selected grids of geomagnetic latitude (N=S):0,10,20,30,40,50,60,70,80,90
%  (3) 5 selected grids of local time: 0, 6, 12, 18, 24.
%  (4) 4 seasonal grids: 1 equinox(SG=90deg), 2 summer (SG=180), 
%                        3 equinox (SG=270), 4 winter(SG=360)
%   SG=season grids=90,180,270,360
%-------------------------------------------------------------------------------

%      DIMENSION CVLEV(4)	
%	  COMMON     /BLOCK1/HMF2,XNMF2,XHMF1,F1REG         
%     *         /QTOP/Y05,H05TOP,QF,XNETOP,XM3000,HHALF,TAU
  persistent CVLEV;
  if isempty(CVLEV)
	  CVLEV = [60.,106.,152.,198.];
  end
%	  LOGICAL F1REG

  ABMLAT=abs(AMLAT);
  IR=floor((COVI-60.)/46.)+1;
%   M1=floor(ABMLAT/10.)+1;
%   L1=floor(TIME/6.)+1;
%   M2=M1+1;
%   if M1 == 10
%     M2=10;
%   end
%   L2=L1+1;
%   if L1 == 5
%     L2=5;
%   end
%
% INTERPOLATE RAT FOR GIVEN RZI
% Call Chebishev approximation to interpolate for given ABMLAT, HRLT
%
  XX = IRI2012.CHEBISH(CVLEV(IR),TIME,ABMLAT,SG);
  if IR == 4
    RAT05=XX;
  else
    YY = IRI2012.CHEBISH(CVLEV(IR+1),TIME,ABMLAT,SG);
    RAT05=XX+(YY-XX)*(COVI-CVLEV(IR))/46.;
  end
  HT05=HMAX*(1.+RAT05);

end

