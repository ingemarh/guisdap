function [ DZ, TZ, TN1, TGN1 ] = DENSU( context,ZH,DLB,TINF,TLB,XM,ALPHA,ZLB,S,MN1,ZN1,TN1,TGN1 )
%DENSU Calculates Temperature and Density Profiles for MSIS models
%      FUNCTION DZ(ALT,DLB,TINF,TLB,XM,ALPHA,TZ,ZLB,S2,
%        MN1,ZN1,TN1,TGN1)
%--------------------------------------------------------------------
%       Calculate Temperature and Density Profiles for MSIS models
%       New lower thermo polynomial 10/30/89
%       INPUT:
%        ZH - altitude (km)
%        DLB - starting density (cm^-3)
%        TINF - temperature at infinity (K)
%        TLB - temperature lower bound (K)
%        XM - molar mass (g/mol)
%        ALPHA - exponent
%        ZLB - altitude at lower bound (km)
%        S - scale (1/km)
%        MN1 - length of ABOVE ZA height array
%        ZN1 - ABOVE ZA height array (km)
%        TN1 - node accelerations at ZN1
%        TGN1 - endpoint accelerations at ZN1
%       OUTPUT:
%        DZ - density (cm^-3)
%        TZ - temperature (K)
%--------------------------------------------------------------------

%      DIMENSION ZN1(MN1),TN1(MN1),TGN1(2),XS(5),YS(5),Y2OUT(5)
%      COMMON/PARMB/GSURF,RE
%      COMMON/LSQV/MP,II,JG,LT,QPB(50),IERR,IFUN,N,J,DV(60)

  RE = context.RE;
  GSURF = context.GSURF;
  if( length(ZH)>1 )
    ALT = ZH;
  else
    ALT = zeros(1,CIRA.DerLast);
    ALT(1,CIRA.Der0) = ZH;
  end
  %        Joining altitude of Bates and spline
  ZA=ZN1(1);
  if ALT(1,CIRA.Der0) > ZA
    Z=ALT;
  else
    Z = zeros(1,CIRA.DerLast);
    Z(1,CIRA.Der0)=ZA;
  end
  [TA,ZG2] = CIRA.BatesTemperature(Z,RE,S,ZLB,TINF,TLB);
  
  if ALT(1,CIRA.Der0) < ZA
    %
    %       CALCULATE TEMPERATURE GRADIENT BELOW ZA
    DTA = CIRA.BatesTemperatureGradient(ZA,RE,S,ZLB,TINF,TA);
    if ALT(1,CIRA.Der0) > ZN1(MN1)
      Z = ALT;
    else
      Z = zeros(1,CIRA.DerLast);
      Z(1,CIRA.Der0) = ZN1(MN1);
    end
    for d=CIRA.Der0:CIRA.DerLast
      TN1(1,d) = TA(1,d);
      TGN1(1,d) = DTA(1,d);
    end
    [TZ,YI,ZGDIF] = CIRA.Temperature(Z,RE,TGN1,TN1,ZN1,MN1);
  else
    TZ = TA;
  end
  if XM ~= 0.0
    %
    %      CALCULATE DENSITY ABOVE ZA
    ZGDIF2 = zeros(1,CIRA.DerLast);
    YI2 = zeros(1,CIRA.DerLast);
    if length(TINF) > 1
      ZGDIF2(1,CIRA.Der0) = 1.0/(S(1,CIRA.Der0)*TINF(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        ZGDIF2(1,d) = ZGDIF2(1,CIRA.Der0)*(-S(1,d)/S(1,CIRA.Der0)-TINF(1,d)/TINF(1,CIRA.Der0));
      end
    else
      ZGDIF2(1,CIRA.Der0) = 1.0/(S(1,CIRA.Der0)*TINF);
      for d=CIRA.Der0+1:CIRA.DerLast
        ZGDIF2(1,d) = ZGDIF2(1,CIRA.Der0)*(-S(1,d)/S(1,CIRA.Der0));
      end
    end
    YI2(1,CIRA.Der0) = S(1,CIRA.Der0)*ZG2(1,CIRA.Der0);
    for d=CIRA.Der0+1:CIRA.DerLast
      YI2(1,d) = S(1,d)*ZG2(1,CIRA.Der0)+S(1,CIRA.Der0)*ZG2(1,d);
    end
    DZ = CIRA.DensityCorrection(DLB,TA,YI2,ALPHA,GSURF,RE,TLB,ZLB,XM,ZGDIF2, ...
      true,-3.912023,Inf);
    
    if ALT(1,CIRA.Der0) <= ZA
      %
      %      CALCULATE DENSITY BELOW ZA
      DZ = CIRA.DensityCorrection(DZ,TZ,YI,ALPHA,GSURF,RE,TN1,ZN1(1),XM,ZGDIF, ...
        false,-Inf,CIRA.ARGMAX);
    end
  else
    DZ = TZ;
  end
end
