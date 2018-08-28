function [ HXL,SCL,AMP,IQUAL ] = INILAY( NIGHT,F1REG,XNMF2,XNMF1,XNME,...
  VNE,HMF2,HMF1,HME,HV1,HV2,HHALF )
%INILAY CALCULATES AMPLITUDES FOR LAY FUNCTIONS
%
%        SUBROUTINE INILAY(NIGHT,F1REG,XNMF2,XNMF1,XNME,VNE,HMF2,HMF1, 
%                                HME,HV1,HV2,HHALF,HXL,SCL,AMP,IQUAL)
%-------------------------------------------------------------------
% CALCULATES AMPLITUDES FOR LAY FUNCTIONS
% D. BILITZA, DECEMBER 1988
%
% INPUT:        NIGHT   LOGICAL VARIABLE FOR DAY/NIGHT DISTINCTION
%               F1REG   LOGICAL VARIABLE FOR F1 OCCURRENCE
%               XNMF2   F2 PEAK ELECTRON DENSITY [M-3]
%               XNMF1   F1 PEAK ELECTRON DENSITY [M-3]
%               XNME    E  PEAK ELECTRON DENSITY [M-3]
%               VNE     ELECTRON DENSITY AT VALLEY BASE [M-3]
%               HMF2    F2 PEAK ALTITUDE [KM]
%               HMF1    F1 PEAK ALTITUDE [KM]
%               HME     E  PEAK ALTITUDE [KM]
%               HV1     ALTITUDE OF VALLEY TOP [KM]
%               HV2     ALTITUDE OF VALLEY BASE [KM]
%               HHALF   ALTITUDE OF HALF-F2-PEAK-DENSITY [KM]
%
% OUTPUT:       HXL(4)  HEIGHT PARAMETERS FOR LAY FUNCTIONS [KM] 
%               SCL(4)  SCALE PARAMETERS FOR LAY FUNCTIONS [KM]
%               AMP(4)  AMPLITUDES FOR LAY FUNCTIONS
%               IQUAL   =0 ok, =1 ok using second choice for HXL(1)
%                       =2 NO SOLUTION
%---------------------------------------------------------------  

%        DIMENSION       XX(8),YY(8),WW(8),AMP(4),HXL(4),SCL(4)
%        LOGICAL         SSIN,NIGHT,F1REG
%
  persistent NUMLAY NC1 ALG102;
  if isempty(NUMLAY)
    % constants --------------------------------------------------------
    NUMLAY=4;
    NC1 = 2;
    ALG102=log10(2.);
  end
  XX = zeros(8,1);
  YY = zeros(8,1);
  WW = zeros(8,1);
  AMP = zeros(NUMLAY,1);
  HXL = zeros(NUMLAY,1);
  SCL = zeros(NUMLAY,1);
  %
  % constraints: xx == height     yy == log(Ne/NmF2)    ww == weights
  % -----------------------------------------------------------------
  ALOGF = log10(XNMF2);
  ALOGEF = log10(XNME) - ALOGF;
  XHALF=XNMF2/2.;
  XX(1) = HHALF;
  XX(2) = HV1;
  XX(3) = HV2;
  XX(4) = HME;
  XX(5) = HME - ( HV2 - HME );
  YY(1) = -ALG102;
  YY(2) = ALOGEF;
  YY(3) = log10(VNE) - ALOGF;
  YY(4) = ALOGEF;
  YY(5) = YY(3);
  YY(7) = 0.0;
  WW(2) = 1.;
  WW(3) = 2.;
  WW(4) = 5.;
  %
  % geometric paramters for LAY -------------------------------------
  % difference to earlier version:  HXL(3) = HV2 + SCL(3)
  %
  SCL0 = 0.7 * ( 0.216 * ( HMF2 - HHALF ) + 56.8 );
  SCL(1) = 0.8 * SCL0;
  SCL(2) = 10.;
  SCL(3) = 9.;
  SCL(4) = 6.;
  HXL(3) = HV2;
  HFFF=HHALF;
  XFFF=XHALF;
  %
  % DAY CONDITION--------------------------------------------------
  % earlier tested:       HXL(2) = HMF1 + SCL(2)
  % 
  if ~NIGHT
    NUMCON = 8;
    HXL(1) = 0.9 * HMF2;
    HXL1T  = HHALF;
    HXL(2) = HMF1;
    HXL(4) = HME - SCL(4);
    XX(6) = HMF1;
    XX(7) = HV2;
    XX(8) = HME;
    YY(8) = 0.0;
    WW(5) = 1.;
    WW(7) = 50.;
    WW(8) = 500.;
    % without F-region ----------------------------------------------
    if ~F1REG
      HXL(2)=(HMF2+HHALF)/2.;
      YY(6) = 0.;
      WW(6) = 0.;
      WW(1) = 1.;
    else
      % with F-region --------------------------------------------
      YY(6) = log10(XNMF1) - ALOGF;
      WW(6) = 3.;
      if (XNMF1-XHALF)*(HMF1-HHALF) < 0.0
        WW(1)=0.5;
      else
        ZET = YY(1) - YY(6);
        WW(1) = IRI2012.EPST( ZET, 0.1, 0.15);
      end
      if HHALF > HMF1
        HFFF=HMF1;
        XFFF=XNMF1;
      else
        HFFF=HHALF;
        XFFF=XHALF;
      end
    end
  else
    %
    % NIGHT CONDITION---------------------------------------------------
    % different HXL,SCL values were tested including: 
    %       SCL(1) = HMF2 * 0.15 - 27.1     HXL(2) = 200.   
    %       HXL(2) = HMF1 + SCL(2)          HXL(3) = 140.
    %       SCL(3) = 5.                     HXL(4) = HME + SCL(4)
    %       HXL(4) = 105.                   
    %
    NUMCON = 7;
    HXL(1) = HHALF;
    HXL1T  = 0.4 * HMF2 + 30.;
    HXL(2) = ( HMF2 + HV1 ) / 2.;
    HXL(4) = HME;
    XX(6) = HV2;
    XX(7) = HME;
    YY(6) = 0.0;
    WW(1) = 1.;
    WW(3) = 3.;
    WW(5) = 0.5;
    WW(6) = 50.;
    WW(7) = 500.;
    HFFF=HHALF;
    XFFF=XHALF;
  end
  %
  % are valley-top and bottomside point compatible ? -------------
  %
  if (HV1-HFFF)*(XNME-XFFF) < 0.0
    WW(2)=0.5;
  end
  if HV1 <= HV2+5.0
    WW(2)=0.5;
  end
  %
  % DETERMINE AMPLITUDES-----------------------------------------
  %
  NC0=NUMCON-NC1;
  IQUAL=0;
  while true
    [AMP,SSIN] = IRI2012.LSKNM(NUMLAY,NUMCON,NC0,NC1,HMF2,SCL,HXL,WW,XX,YY);
    if IQUAL > 0
      break;
    end
    if (abs(AMP(1)) > 10.0) || (SSIN)
      IQUAL=1;
      HXL(1)=HXL1T;
    else
      break;
    end
  end
  if SSIN
    IQUAL=2;
  end

end

