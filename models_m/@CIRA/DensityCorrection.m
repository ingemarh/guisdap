function [DZ] = DensityCorrection(DZ1,TZ,YI,ALPHA,GSURF,RE,TN,ZN,XM,ZGDIF, ...
  addGamma,lowLim,highLim)
  % correction to density
  % INPUT:
  %   DZ1 - input density (1/cm^3)
  %   TZ - temperature at altitude (K)
  %   YI - (1/K)
  %   ALPHA - exponent constant
  %   GSURF - surface gravity (cm/s^2)
  %   RE - radius of Earth (km)
  %   TN - temperature at ZN
  %   ZN - altitude (km)
  %   XM - molar mass (g/mol)
  %   ZGDIF - scale
  %   addGamma - flag whether to add gamma calculation to exponent
  %   lowLim - lower limit for exponent
  %   highLim - upper limit for exponent
  % OUTPUT:
  %   DZ - corrected density (1/cm^3)
  GLB = zeros(1,CIRA.DerLast);
  GAMM = zeros(1,CIRA.DerLast);
  DZ = zeros(1,CIRA.DerLast);
  a = ZN/RE(1,CIRA.Der0);
  GLB(1,CIRA.Der0)=GSURF(1,CIRA.Der0)/(1.+a)^2;
  GAMM(1,CIRA.Der0)=XM*GLB(1,CIRA.Der0)*ZGDIF(1,CIRA.Der0)/CIRA.RGAS; % K/km * [Z]
  for d=CIRA.Der0+1:CIRA.DerLast
    GLB(1,d)=GSURF(1,d)/(1.+a)^2 ...
      +2*GLB(1,CIRA.Der0)*a*RE(1,d)/(RE(1,CIRA.Der0)+ZN);
    GAMM(1,d)=(GLB(1,d)*ZGDIF(1,CIRA.Der0)+GLB(1,CIRA.Der0)*ZGDIF(1,d)) ...
      *XM/CIRA.RGAS;
  end
  EXPL = zeros(1,CIRA.DerLast);
  EXPL(1,CIRA.Der0)=GAMM(1,CIRA.Der0)*YI(1,CIRA.Der0);
  if TZ(1,CIRA.Der0) <= 0. % catch division by zero
    EXPL(1,CIRA.Der0) = lowLim;
  elseif EXPL(1,CIRA.Der0) < lowLim
    EXPL(1,CIRA.Der0) = lowLim;
  elseif EXPL(1,CIRA.Der0) > highLim
    EXPL(1,CIRA.Der0) = highLim;
  else
    for d=CIRA.Der0+1:CIRA.DerLast
      EXPL(1,d)=GAMM(1,CIRA.Der0)*YI(1,d)+GAMM(1,d)*YI(1,CIRA.Der0);
    end
  end
  %       Density at altitude
  f = zeros(1,CIRA.DerLast);
  if nargin < 11 || addGamma == false
    a = 1.+ALPHA;
    if length(TN) > 1 && length(TZ) > 1
      f(1,CIRA.Der0) = (TN(1,CIRA.Der0)/TZ(1,CIRA.Der0))^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*( ...
          +a*(TN(1,d)/TN(1,CIRA.Der0)-TZ(1,d)/TZ(1,CIRA.Der0))-EXPL(1,d));
      end
    elseif length(TZ) > 1
      f(1,CIRA.Der0) = (TN/TZ(1,CIRA.Der0))^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*( ...
          +a*(-TZ(1,d)/TZ(1,CIRA.Der0))-EXPL(1,d));
      end
    elseif length(TN) > 1
      f(1,CIRA.Der0) = (TN(1,CIRA.Der0)/TZ)^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*( ...
          +a*(TN(1,d)/TN(1,CIRA.Der0))-EXPL(1,d));
      end
    else
      f(1,CIRA.Der0) = (TN/TZ)^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*(-EXPL(1,d));
      end
    end
  else
    a = 1.+ALPHA+GAMM(1,CIRA.Der0);
    if length(TN) > 1 && length(TZ) > 1
      lf = log(TN(1,CIRA.Der0)/TZ(1,CIRA.Der0));
      f(1,CIRA.Der0) = (TN(1,CIRA.Der0)/TZ(1,CIRA.Der0))^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*(lf*GAMM(1,d) ...
          +a*(TN(1,d)/TN(1,CIRA.Der0)-TZ(1,d)/TZ(1,CIRA.Der0))-EXPL(1,d));
      end
    elseif length(TZ) > 1
      f(1,CIRA.Der0) = (TN/TZ(1,CIRA.Der0))^a*exp(-EXPL(1,CIRA.Der0));
      lf = log(TN/TZ(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*(lf*GAMM(1,d) ...
          +a*(-TZ(1,d)/TZ(1,CIRA.Der0))-EXPL(1,d));
      end
    elseif length(TN) > 1
      lf = log(TN(1,CIRA.Der0)/TZ);
      f(1,CIRA.Der0) = (TN(1,CIRA.Der0)/TZ)^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*(lf*GAMM(1,d) ...
          +a*(TN(1,d)/TN(1,CIRA.Der0))-EXPL(1,d));
      end
    else
      lf = log(TN/TZ);
      f(1,CIRA.Der0) = (TN/TZ)^a*exp(-EXPL(1,CIRA.Der0));
      for d=CIRA.Der0+1:CIRA.DerLast
        f(1,d) = f(1,CIRA.Der0)*(lf*GAMM(1,d)-EXPL(1,d));
      end
    end
  end
  if length(DZ1) > 1
    DZ(1,CIRA.Der0) = DZ1(1,CIRA.Der0)*f(1,CIRA.Der0);
    for d=CIRA.Der0+1:CIRA.DerLast
      DZ(1,d) = DZ1(1,d)*f(1,CIRA.Der0)+DZ1(1,CIRA.Der0)*f(1,d);
    end
  else
    for d=CIRA.Der0:CIRA.DerLast
      DZ(1,d) = DZ1*f(1,d);
    end
  end
end
