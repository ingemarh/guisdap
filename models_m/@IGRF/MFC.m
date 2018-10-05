function [ H,D,Z ] = MFC( context, SLA,SLO,R )
%MFC Computation of the IGRF magnetic field components
%        SUBROUTINE MFC(SLA,SLO,R,H,D,Z)
%  *********************************************************************
%  Computation of the IGRF magnetic field components
%  Extracted as a subroutine from the earlier version of GEO-CGM.FOR
%  V. Papitashvili, February 1999
%  *********************************************************************

%      COMMON /NM/NM
%      COMMON /IYR/IYR
%
  %  This takes care if SLA or CLA are dummy values (e.g., 999.99)

  if SLA == IGRF.BAD_ANGLE
    %X = 99999.;
    %Y = 99999.;
    Z = 99999.;
    H = 99999.;
    D = IGRF.BAD_ANGLE;
    %I = IGRF.BAD_ANGLE;
    %F = 99999.;
    return;
  end

  %  Computation of all geomagnetic field components
  RLA = (90.-SLA)*IGRF.UMR;
  RLO = SLO*IGRF.UMR;
  [BR,BT,BF] = IGRF.IGRFM(context.IYR,context.NM,R,RLA,RLO);
  X = -BT;
  Y =  BF;
  Z = -BR;
  H = sqrt(X^2+Y^2);
  if X == 0 && Y == 0
    D = 0.0;
  else
    D = atan2(Y,X)/IGRF.UMR;
  end
  %if Z == 0 && H == 0
    %I = 0.0;
  %else
    %I = atan2(Z,H)/IGRF.UMR;
  %end
  %F = sqrt(H^2+Z^2);


end

