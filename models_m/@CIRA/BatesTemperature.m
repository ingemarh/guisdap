function [TT,ZG2] = BatesTemperature(Z,RE,S,ZLB,TINF,TLB)
  % Bates temperature
  % ZG2 = (Z-ZLB)*(RE+ZLB)/(RE+Z);
  % T = TINF-(TINF-TLB)*exp(-S*ZG2)
  % INPUT:
  %   Z - altitude (km)
  %   RE - radius of Earth at location (km)
  %   S - scale (1/km)
  %   ZLB - lower bound altitude (km)
  %   TINF - temperature at infinite altitude (K)
  %   TLB - temperature at ZLB (K)
  % OUTPUT:
  %   TT - temperature at Z (K)
  %   ZG2 - Geopotential altitude difference from ZLB (km)
  
  % Geopotential altitude difference from ZLB
  nd = CIRA.DerLast;
  ZG2 = zeros(1,nd);
  if length(Z) > 1 && length(RE) > 1
    [ZG2(1,1),ZG2dRE,ZG2dZ]=CIRA.ZETA(RE(1,1),Z(1,1),ZLB);
    for d=2:nd
      ZG2(1,d) = ZG2dRE*RE(1,d) + ZG2dZ*Z(1,d);
    end
  elseif length(RE) > 1
    [ZG2(1,1),ZG2dRE]=CIRA.ZETA(RE(1,1),Z,ZLB);
    for d=2:nd
      ZG2(1,d) = ZG2dRE*RE(1,d);
    end
  elseif length(Z) > 1
    [ZG2(1,1),~,ZG2dZ]=CIRA.ZETA(RE,Z(1,1),ZLB);
    for d=2:nd
      ZG2(1,d) = ZG2dZ*Z(1,d);
    end
  else
    [ZG2(1,1)]=CIRA.ZETA(RE,Z,ZLB);
    for d=2:nd
      ZG2(1,d) = 0;
    end
  end
  EXPL = zeros(1,nd);
  if length(S) > 1
    EXPL(1,1) = exp(-S(1,1)*ZG2(1,1));
    for d=2:nd
      EXPL(1,d) = EXPL(1,1)*(-S(1,d)*ZG2(1,1)-S(1,1)*ZG2(1,d));
    end
  else
    EXPL(1,1) = exp(-S*ZG2(1,1));
    for d=2:nd
      EXPL(1,d) = EXPL(1,1)*(-S*ZG2(1,d));
    end
  end
  
  TT = zeros(1,CIRA.DerLast);
  if length(TINF) > 1 && length(TLB) > 1
    TT(1,1) = TINF(1,1)-(TINF(1,1)-TLB(1,1))*EXPL(1,CIRA.Der0);
    for d=2:nd
      TT(1,d) = TINF(1,d)-(TINF(1,d)-TLB(1,d))*EXPL(1,CIRA.Der0) ...
                         -(TINF(1,CIRA.Der0)-TLB(1,CIRA.Der0))*EXPL(1,d);
    end
  elseif length(TLB) > 1
    TT(1,1)=TINF-(TINF-TLB(1,1))*EXPL(1,1);
    for d=2:nd
      TT(1,d)=-(-TLB(1,d))*EXPL(1,1)-(TINF-TLB(1,1))*EXPL(1,d);
    end
  elseif length(TINF) > 1
    TT(1,1)=TINF(1,1)-(TINF(1,1)-TLB)*EXPL(1,1);
    for d=2:nd
      TT(1,d)=TINF(1,d)-(TINF(1,d))*EXPL(1,CIRA.Der0)-(TINF(1,CIRA.Der0)-TLB)*EXPL(1,d);
    end
  else
    TT(1,1) = TINF-(TINF-TLB)*EXPL(1,1);
    for d=2:nd
      TT(1,d) = -(TINF-TLB)*EXPL(1,d);
    end
  end
end
