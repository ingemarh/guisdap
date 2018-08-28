function [TZ,YI,ZGDIF] = Temperature(Z,RE,TGN,TN,ZN,MN)
  % calculate spline interpolated temperature at altitude
  % INPUT:
  %   Z - altitude (km)
  %   RE - radius of Earth (km)
  %   TGN - endpoint accelerations
  %   TN - node accelerations
  %   ZN - array of altitudes (km)
  %   MN - size of ZN and TN array
  % OUTPUT:
  %   TZ - spline interpolated temperature (K)
  %   YI - integration of inverse temperatures along spline up to Z
  %   ZGDIF - geopotental altitude difference from ZN(1)
  TZ = zeros(1,CIRA.DerLast);
  %      Geopotental difference from Z1
  ZG = zeros(1,CIRA.DerLast);
  if length(RE) > 1 && length(Z) > 1
    [ZG(1,CIRA.Der0),ZGdR,ZGdZ]=CIRA.ZETA(RE(1,CIRA.Der0),Z(1,CIRA.Der0),ZN(1));
    for d=CIRA.Der0+1:CIRA.DerLast
      ZG(1,d) = ZGdR*RE(1,d) + ZGdZ*Z(1,d);
    end
  elseif length(RE) > 1
    [ZG(1,CIRA.Der0),ZGdR]=CIRA.ZETA(RE(1,CIRA.Der0),Z,ZN(1));
    for d=CIRA.Der0+1:CIRA.DerLast
      ZG(1,d) = ZGdR*RE(1,d);
    end
  elseif length(Z) > 1
    [ZG(1,CIRA.Der0),~,ZGdZ]=CIRA.ZETA(RE,Z(1,CIRA.Der0),ZN(1));
    for d=CIRA.Der0+1:CIRA.DerLast
      ZG(1,d) = ZGdZ*Z(1,d);
    end
  else
    [ZG(1,CIRA.Der0)]=CIRA.ZETA(RE,Z,ZN(1));
    for d=CIRA.Der0+1:CIRA.DerLast
      ZG(1,d) = 0.0;
    end
  end
  ZGDIF = zeros(1,CIRA.DerLast);
  if length(RE) > 1
    [ZGDIF(1,CIRA.Der0),ZGDIFdR]=CIRA.ZETA(RE(1,CIRA.Der0),ZN(MN),ZN(1));
    for d=CIRA.Der0+1:CIRA.DerLast
      ZGDIF(1,d) = ZGDIFdR*RE(1,d);
    end
  else
    [ZGDIF(1,CIRA.Der0)]=CIRA.ZETA(RE,ZN(MN),ZN(1));
    for d=CIRA.Der0+1:CIRA.DerLast
      ZGDIF(1,d) = 0.0;
    end
  end
  [XS,YS] = SplineNodes(RE,TN,ZN,MN,ZGDIF);
  YD1 = FirstEndpoint(TN,TGN,ZGDIF);
  YD2 = SecondEndpoint(RE,TN,ZN,MN,TGN,ZGDIF);
  X = zeros(1,CIRA.DerLast);
  X(1,CIRA.Der0)=ZG(1,CIRA.Der0)/ZGDIF(1,CIRA.Der0);
  for d=CIRA.Der0+1:CIRA.DerLast
    X(1,d)=ZG(1,d)/ZGDIF(1,CIRA.Der0) ...
      - X(1,CIRA.Der0)*ZGDIF(1,d)/ZGDIF(1,CIRA.Der0);
  end
  [Y,YI] = CIRA.SPLINE(XS,YS,1,MN,1,YD1,YD2,X);
  TZ(1,CIRA.Der0)=1./Y(1,CIRA.Der0);
  for d=CIRA.Der0+1:CIRA.DerLast
    TZ(1,d)=-Y(1,d)*TZ(1,CIRA.Der0)*TZ(1,CIRA.Der0);
  end
end
function [XS,YS] = SplineNodes(RE,TN,ZN,MN,ZGDIF)
  %       Set up spline nodes
  XS = zeros(MN,CIRA.DerLast);
  YS = zeros(MN,CIRA.DerLast);
  if length(RE) > 1
    for K=1:MN
      [ZG3,ZG3dR] = CIRA.ZETA(RE(1,CIRA.Der0),ZN(K),ZN(1));
      XS(K,CIRA.Der0)=ZG3/ZGDIF(1,CIRA.Der0);
      YS(K,CIRA.Der0)=1./TN(K,CIRA.Der0);
      for d=CIRA.Der0+1:CIRA.DerLast
        XS(K,d)=ZG3dR*RE(1,d)/ZGDIF(1,CIRA.Der0) ...
          -XS(K,CIRA.Der0)*ZGDIF(1,d)/ZGDIF(1,CIRA.Der0);
        YS(K,d)=-TN(K,d)*YS(K,CIRA.Der0)*YS(K,CIRA.Der0);
      end
    end
  else
    for K=1:MN
      [ZG3] = CIRA.ZETA(RE,ZN(K),ZN(1));
      XS(K,CIRA.Der0)=ZG3/ZGDIF(1,CIRA.Der0);
      YS(K,CIRA.Der0)=1./TN(K,CIRA.Der0);
      for d=CIRA.Der0+1:CIRA.DerLast
        XS(K,d)=-XS(K,CIRA.Der0)*ZGDIF(1,d)/ZGDIF(1,CIRA.Der0);
        YS(K,d)=-TN(K,d)*YS(K,CIRA.Der0)*YS(K,CIRA.Der0);
      end
    end
  end
end
function YD1 = FirstEndpoint(TN,TGN,ZGDIF)
  YD1 = zeros(1,CIRA.DerLast);
  T12 = TN(1,CIRA.Der0)*TN(1,CIRA.Der0);
  YD1(1,CIRA.Der0)=-TGN(1,CIRA.Der0)/T12*ZGDIF(1,CIRA.Der0);
  for d=CIRA.Der0+1:CIRA.DerLast
    YD1(1,d)=-TGN(1,d)/T12*ZGDIF(1,CIRA.Der0) ...
      -2.0*YD1(1,CIRA.Der0)*TN(1,d)/TN(1,CIRA.Der0) ...
      -TGN(1,CIRA.Der0)/T12*ZGDIF(1,d);
  end
end
function YD2 = SecondEndpoint(RE,T2,ZN,MN,TGN,ZGDIF)
  YD2 = zeros(1,CIRA.DerLast);
  if length(RE) > 1
    a = RE(1,CIRA.Der0)+ZN(MN);
    b = RE(1,CIRA.Der0)+ZN(1);
    f = (a/b)^2;
    T22 = T2(MN,CIRA.Der0)*T2(MN,CIRA.Der0);
    fdRE = 2*(a/b)*(1.0-a/b)/b;
    YD2(1,CIRA.Der0)=-TGN(2,CIRA.Der0)/T22*ZGDIF(1,CIRA.Der0)*f;
    for d=CIRA.Der0+1:CIRA.DerLast
      YD2(1,d)=-TGN(2,d)/T22*ZGDIF(1,CIRA.Der0)*f ...
        -2.0*YD2(1,CIRA.Der0)*T2(MN,d)/T2(MN,CIRA.Der0)...
        -TGN(2,CIRA.Der0)/T22 ...
        *(ZGDIF(1,d)*f + ZGDIF(1,CIRA.Der0)*fdRE*RE(1,d));
    end
  else
    a = RE+ZN(MN);
    b = RE+ZN(1);
    f = (a/b)^2;
    T22 = T2(MN,CIRA.Der0)*T2(MN,CIRA.Der0);
    YD2(1,CIRA.Der0)=-TGN(2,CIRA.Der0)/T22*ZGDIF(1,CIRA.Der0)*f;
    for d=CIRA.Der0+1:CIRA.DerLast
      YD2(1,d)=-TGN(2,d)/T22*ZGDIF(1,CIRA.Der0)*f ...
        -2.0*YD2(1,CIRA.Der0)*T2(MN,d)/T2(MN,CIRA.Der0)...
        -TGN(2,CIRA.Der0)/T22*ZGDIF(1,d)*f;
    end
  end
end

