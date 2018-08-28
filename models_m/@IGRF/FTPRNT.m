function [ ACLA,ACLO,SLAF,SLOF ] = FTPRNT( context, RH,SLA,SLO,CLA,CLO,RF )
%FTPRNT Calculation of the magnetic field line footprint at the Earth's surface
%      SUBROUTINE FTPRNT(RH,SLA,SLO,CLA,CLO,ACLA,ACLO,SLAF,SLOF,RF)
%  *********************************************************************
%  Calculation of the magnetic field line footprint at the Earth's
%  (or any higher) surface.
%  Extracted as a subroutine from the earlier version of GEO-CGM.FOR by
%  V. Papitashvili in February 1999 but then the subroutine was revised
%  to obtain the Altitude Adjusted CGM coordinates. The AACGM approach
%  is proposed by Kile Baker of the JHU/APL, see their World Wide Web
%  site http://sd-www.jhuapl.edu/RADAR/AACGM/ for details.
%  If RF = 1-Re (i.e., at the Earth's surface), then the footprint
%  location is defined as the Altitude Adjusted (AA) CGM coordinates
%  for a given point (ACLA, ACLO).
%
%  If RF = 1.xx Re (i.e., at any altitude above or below the starting
%  point), then the conjunction between these two points can be found
%  along the field line.
%  *********************************************************************

%      COMMON /NM/NM
%      COMMON /IYR/IYR

%  This takes care if SLA or CLA are dummy values (e.g., 999.99)

  if SLA == IGRF.BAD_ANGLE || CLA == IGRF.BAD_ANGLE || RF == RH
    ACLA = IGRF.BAD_ANGLE;
    ACLO = IGRF.BAD_ANGLE;
    SLAF = IGRF.BAD_ANGLE;
    SLOF = IGRF.BAD_ANGLE;
    return;
  end

%  Defining the Altitude Adjusted CGM coordinates for a given point

  COL = (90. - CLA)*IGRF.UMR;
  SN2 = (sin(COL))^2;
  ACOL = asin(sqrt((SN2*RF)/RH));
  ACLA = 90. - ACOL/IGRF.UMR;
  if CLA < 0.
    ACLA = -ACLA;
  end
  ACLO = CLO;

  [SLAF,SLOF,~,~,~] = context.CORGEO(RF,ACLA,ACLO);

  if SLAF ~= IGRF.BAD_ANGLE
    return;
  end

%  Tracing the magnetic field line down to the Earth's surface at low
%  latitudes if CORGEO failed to calculate geocentric coordinates SLAF
%  and SLOF

  if SN2 < 0.0000001
    SN2 = 0.0000001;
  end
  RL = RH/SN2;
  FRAC = 0.03/(1.+3./(RL-0.6));

%  Checking direction of the magnetic field-line, so the step along
%  the field-line will go down, to the Earth surface

  if CLA >= 0.
    FRAC = -FRAC;
  end
  DS = RH*FRAC;


%  Start from an initial point

  R = RH;
  RSLA = (90. - SLA)*IGRF.UMR;
  RSLO = SLO*IGRF.UMR;
  [XF,YF,ZF] = IGRF.SPHCAR(R,RSLA,RSLO,IGRF.SPHERICAL_INPUT);
  RF1 = R;
  XF1 = XF;
  YF1 = YF;
  ZF1 = ZF;
  while true
    [XF,YF,ZF] = context.SHAG(XF,YF,ZF,DS);
    RR = sqrt(XF^2+YF^2+ZF^2);
    if RR > RH
      DS = -DS;
      %XF = XF1;
      %YF = YF1;
      %ZF = ZF1;
      R = RH;
      RSLA = (90. - SLA)*IGRF.UMR;
      RSLO = SLO*IGRF.UMR;
      [XF,YF,ZF] = IGRF.SPHCAR(R,RSLA,RSLO,IGRF.SPHERICAL_INPUT);
      RF1 = R;
      XF1 = XF;
      YF1 = YF;
      ZF1 = ZF;
      continue;
    end
    if RR > RF
      RF1 = RR;
      XF1 = XF;
      YF1 = YF;
      ZF1 = ZF;
      continue;
    else
      DR1 = abs(RF1 - RF);
      DR0 = abs( RF - RR);
      DR10 = DR1 + DR0;
      if DR10 ~= 0.
         DS = DS*(DR1/DR10);
         [XF1,YF1,ZF1] = context.SHAG(XF1,YF1,ZF1,DS);
      end
      [~,SLAF,SLOF] = IGRF.SPHCAR(XF1,YF1,ZF1,IGRF.CARTESIAN_INPUT);
      SLAF = 90. - SLAF/IGRF.UMR;
      SLOF = SLOF/IGRF.UMR;
    end
    break;
  end


end

