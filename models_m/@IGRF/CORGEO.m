function [ SLA,SLO,DLA,DLO,PMI ] = CORGEO( context, RH,CLA,CLO )
%CORGEO Calculates geocentric coordinates from corrected geomagnetic ones
%
%      SUBROUTINE CORGEO(SLA,SLO,RH,DLA,DLO,CLA,CLO,PMI)
%  *********************************************************************
%  Calculates geocentric coordinates from corrected geomagnetic ones.
%  The code is written by Vladimir Popov and Vladimir Papitashvili
%  in mid-1980s; revised by V. Papitashvili in February 1999
%  *********************************************************************

%      COMMON /NM/NM
%      COMMON /IYR/IYR
%
%  This takes care if CLA is a dummy value (e.g., 999.99)

  jc = 0;
  if abs(CLA) < 0.1
    if context.KONSOL > 0
      fprintf(context.KONSOL, ...
        'WARNING - No calculations within +/-0.1 degree near CGM equator\n');
    end
    jc = 1;
  end
  if CLA == IGRF.BAD_ANGLE || jc == 1
    SLA = IGRF.BAD_ANGLE;
    SLO = IGRF.BAD_ANGLE;
    DLA = IGRF.BAD_ANGLE;
    DLO = IGRF.BAD_ANGLE;
    PMI = IGRF.BAD_ANGLE;
    return;
  end

  NG = context.NM;

  COL = 90. - CLA;
  R = 10.;
  R1 = R;
  R0 = R;
  COL = COL*IGRF.UMR;
  RLO = CLO*IGRF.UMR;
  SN = sin(COL);
  SN2 = SN*SN;

  %  The CGM latitude should be at least 0.01 deg. away of the CGM pole

  if SN2 < 0.000000003
    SN2 = 0.000000003;
  end
  %      RFI = 1./SN2
  RFI = RH/SN2;
  PMI = RFI;
  if PMI > 99.999
    PMI = IGRF.BAD_ANGLE;
  end
  AA10 = R/RFI;

  %  RFI = R if COL = 90 deg.

  if RFI > R
    SAA = AA10/(1.-AA10);
    SAQ = sqrt(SAA);
    SCLA = atan(SAQ);
    if CLA < 0
      SCLA = IGRF.pi - SCLA;
    end

  else

    SCLA = IGRF.pi/2;
    R0 = RFI;
  end
  [XM,YM,ZM] = IGRF.SPHCAR(R0,SCLA,RLO,IGRF.SPHERICAL_INPUT);
  [X,Y,Z] = context.GEOMAG(XM,YM,ZM,IGRF.GEOMAGNETIC_COORDINATES);
  RL = R0;
  FRAC = -0.03/(1. + 3./(RL - 0.6));
  if CLA < 0.
    FRAC = -FRAC;
  end
  R = R0;
  X1 = 0;
  Y1 = 0;
  Z1 = 0;
  while true
    DS = R*FRAC;
    context.NM = (1. + 9./R) + 0.5;
    [X,Y,Z] = context.SHAG(X,Y,Z,DS);
    R = sqrt(X^2+Y^2+Z^2);
    if R <= RH
      break;
    end
    R1 = R;
    X1 = X;
    Y1 = Y;
    Z1 = Z;
  end

  %  Define intersection with the start surface

  DR1 = abs(RH - R1);
  DR0 = abs(RH - R);
  DR10 = DR1 + DR0;
  if DR10 ~= 0.
    DS = DS*(DR1/DR10);
    [X1,Y1,Z1] = context.SHAG(X1,Y1,Z1,DS);
  end

  [~,GTET,GXLA] = IGRF.SPHCAR(X1,Y1,Z1,IGRF.CARTESIAN_INPUT);
  GTH = GTET/IGRF.UMR;
  SLO = GXLA/IGRF.UMR;
  SLA = 90. - GTH;
  [XM,YM,ZM] = context.GEOMAG(X1,Y1,Z1,IGRF.GEOGRAPHIC_COORDINATES);
  [~,TH,PF] = IGRF.SPHCAR(XM,YM,ZM,IGRF.CARTESIAN_INPUT);
  DLO = PF/IGRF.UMR;
  DLA = 90. - TH/IGRF.UMR;

  context.NM = NG;

  %  Because CORGEO cannot check if the CGM --> GEO transformation is
  %  performed correctly in the equatorial area (that is, where the IGRF
  %  field line may never cross the dipole equatorial plane). Therefore,
  %  the backward check is required for geocentric latitudes lower than
  %  30 degrees (see the paper referenced in GEOLOW)

  if abs(SLA) < 30. || abs(CLA) < 30.
    [~,~,CLAS,~,~] = context.GEOCOR(SLA,SLO,RH);

    if CLAS == IGRF.BAD_ANGLE
      [CLAS,~,~,~,~] = context.GEOLOW(SLA,SLO,RH);
    end
    if abs(abs(CLA)-abs(CLAS)) >= 1.
      if context.KONSOL > 0
        fprintf(context.KONSOL, ...
          'WARNING - Selected CGM_Lat.=%6.2f,CGM_Lon.=%6.2f is too close to geomagnetic equator where CGM coordinates are not defined\n', CLA,CLO);
      end
      SLA = IGRF.BAD_ANGLE;
      SLO = IGRF.BAD_ANGLE;
      PMI = IGRF.BAD_ANGLE;
    end
  end

end

