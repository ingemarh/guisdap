function [ DZ, TZ ] = DENSM( context, ZH,D0,XM,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2 )
%DENSM Calculate Temperature and Density Profiles for lower atmosphere
%      FUNCTION DENSM(ALT,D0,XM,TZ,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2)
%--------------------------------------------------------------------
%       Calculate Temperature and Density Profiles for lower atmos.
%        ZH - altitude (km)
%        D0 - starting density (g/cm^3)
%        XM - molar mass (g/mol)
%        MN3 - length of ZN3
%        ZN3 - TROPOSPHERE/STRATOSPHERE height array (km)
%        TN3 - node accelerations at ZN3
%        TGN3 - endpoint accelerations at ZN3
%        MN2 - length of ZN2
%        ZN2 - STRATOSPHERE/MESOSPHERE height array (km)
%        TN2 - node accelerations at ZN2
%        TGN2 - endpoint accelerations at ZN2
%--------------------------------------------------------------------

%      DIMENSION ZN3(MN3),TN3(MN3),TGN3(2),XS(10),YS(10),Y2OUT(10)
%      DIMENSION ZN2(MN2),TN2(MN2),TGN2(2)
%      COMMON/PARMB/GSURF,RE
%      COMMON/FIT/TAF
%      COMMON/LSQV/MP,II,JG,LT,QPB(50),IERR,IFUN,N,J,DV(60)
%      SAVE
  RE = context.RE;
  GSURF = context.GSURF;
  if length(ZH) > 1
    ALT = ZH;
  else
    ALT = zeros(1,CIRA.DerLast);
    ALT(1,CIRA.Der0) = ZH;
  end
  if length(D0) > 1
    DZ = D0;
  else
    DZ = zeros(1,CIRA.DerLast);
    DZ(1,CIRA.Der0) = D0;
  end
  if ALT(1,CIRA.Der0) <= ZN2(1)
    %      STRATOSPHERE/MESOSPHERE TEMPERATURE
    if ALT(1,CIRA.Der0) > ZN2(MN2)
      Z = ALT;
    else
      Z = zeros(1,CIRA.DerLast);
      Z(1,CIRA.Der0) = ZN2(MN2);
    end
    [TZ,YI,ZGDIF] = CIRA.Temperature(Z,RE,TGN2,TN2,ZN2,MN2);
    if XM ~= 0.0
      %
      %      CALCULATE STRATOSPHERE/MESOSPHERE DENSITY 
      DZ = CIRA.DensityCorrection(DZ,TZ,YI,0.0,GSURF,RE,TN2,ZN2(1),XM,ZGDIF, ...
        false,-Inf,CIRA.ARGMAX);
    end
    if ALT(1,CIRA.Der0) <= ZN3(1)
      %
      %      TROPOSPHERE/STRATOSPHERE TEMPERATURE
      Z=ALT;
      [TZ,YI,ZGDIF] = CIRA.Temperature(Z,RE,TGN3,TN3,ZN3,MN3);
      if XM ~= 0.0
        %
        %      CALCULATE TROPOSPHERIC/STRATOSPHERE DENSITY 
        %     
        DZ = CIRA.DensityCorrection(DZ,TZ,YI,0.0,GSURF,RE,TN3,ZN3(1),XM,ZGDIF, ...
          false,-Inf,CIRA.ARGMAX);
      end
    end
  else
    TZ = zeros(1,CIRA.DerLast);
  end
  if XM == 0.0
    DZ=TZ;
  end
end

