function [ DLA,DLO,CLA,CLO,PMI ] = GEOCOR( context, SLA,SLO,RH )
%GEOCOR Calculates corrected geomagnetic coordinates from geocentric ones
%      SUBROUTINE GEOCOR(SLA,SLO,RH,DLA,DLO,CLA,CLO,PMI)
%  *********************************************************************
%  Calculates corrected geomagnetic coordinates from geocentric ones
%  The code is written by Vladimir Popov and Vladimir Papitashvili
%  in mid-1980s; revised by V. Papitashvili in February 1999
%  *********************************************************************

%      COMMON /NM/NM
%      COMMON /IYR/IYR

  %  This takes care if SLA is a dummy value (e.g., 999.99)

  if SLA == IGRF.BAD_ANGLE
    CLA = IGRF.BAD_ANGLE;
    CLO = IGRF.BAD_ANGLE;
    DLA = IGRF.BAD_ANGLE;
    DLO = IGRF.BAD_ANGLE;
    PMI = IGRF.BAD_ANGLE;
    return;
  end

  NG = context.NM;

  COL = 90. - SLA;
  R = RH;
  %R1 = R;
  COL = COL*IGRF.UMR;
  RLO = SLO*IGRF.UMR;
  [X,Y,Z] = IGRF.SPHCAR(R,COL,RLO,IGRF.SPHERICAL_INPUT);
  [XM,YM,ZM] = context.GEOMAG(X,Y,Z,IGRF.GEOGRAPHIC_COORDINATES);
  [~,TH,PF] = IGRF.SPHCAR(XM,YM,ZM,IGRF.CARTESIAN_INPUT);
  SZM = ZM;
  DLO = PF/IGRF.UMR;
  DCO = TH/IGRF.UMR;
  DLA = 90. - DCO;
  RL = R/(sin(TH))^2;
  FRAC = 0.03/(1. + 3./(RL - 0.6));

  if SZM < 0.
    FRAC = -FRAC;
  end

  %  Error to determine the dipole equtorial plane: aprox. 0.5 arc min

  HHH = 0.0001571;

  %  Trace the IGRF magnetic field line to the dipole equatorial plane

  DS = R*FRAC;
  DCL = 999.0;
  while abs(DCL) > HHH
    context.NM = (1. + 9./R) + 0.5;
    R1 = R;
    X1 = X;
    Y1 = Y;
    Z1 = Z;
    [X,Y,Z] = context.SHAG(X,Y,Z,DS);
    [XM,YM,ZM] = context.GEOMAG(X,Y,Z,IGRF.GEOGRAPHIC_COORDINATES);
    [R,C,~] = IGRF.SPHCAR(XM,YM,ZM,IGRF.CARTESIAN_INPUT);
    if RH < R && R <= 10.+RH
      DCL = C - IGRF.pi/2;
      if abs(DCL) > HHH
        RZM = ZM;
        if (SZM > 0. && RZM > 0.) || (SZM < 0. && RZM < 0.)
          DS = R*FRAC;
        else
          R = R1;
          X = X1;
          Y = Y1;
          Z = Z1;
          DS = DS/2.;
        end
      end
    else
      break;
    end
  end
  %  As tracing goes above (RH+10_Re), use the dipole field line

  if R <= 10.+RH

    %  If the field line returns to the start surface without crossing the
    %  dipole equatorial plane, no CGM coordinates can be calculated

    if R > RH

      DCL = C - IGRF.pi/2;
      if abs(DCL) > HHH
        RZM = ZM;
        if (SZM > 0. && RZM > 0.) || (SZM < 0. && RZM < 0.)
          %DS = R*FRAC;
        else
          %R = R1;
          X = X1;
          Y = Y1;
          Z = Z1;
          %DS = DS/2.;
        end
      end
    end
    [XM,YM,ZM] = context.GEOMAG(X,Y,Z,IGRF.GEOGRAPHIC_COORDINATES);
    [R,GTET,GXLA] = IGRF.SPHCAR(XM,YM,ZM,IGRF.CARTESIAN_INPUT);
    ST = abs(sin(GTET));
    den = R - RH*ST^2;
    if den ~= 0
      RRH = abs(RH/den);
      CLA = IGRF.pi/2 - atan(ST*sqrt(RRH));
    else
      CLA = 0.0;
    end
    CLA = CLA/IGRF.UMR;
    CLO = GXLA/IGRF.UMR;
    if SZM < 0.
      CLA = -CLA;
    end
    SSLA = 90. - CLA;
    SSLA = SSLA*IGRF.UMR;
    SN = sin(SSLA);
    %PMI = 1/(SN*SN)
    PMI = RH/(SN*SN);
  else

    CLA = IGRF.BAD_ANGLE;
    CLO = IGRF.BAD_ANGLE;
    PMI = IGRF.BAD_ANGLE;
  end
  context.NM = NG;


end

