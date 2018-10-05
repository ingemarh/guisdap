function DTA = BatesTemperatureGradient(Z,RE,S,ZLB,TINF,TA)
  % Temperature gradient at Z from Bates profile
  % dTAdZ=(TINF-TA)*S*((RE+ZLB)/(RE+Z))^2
  % INPUT:
  %   Z - altitude (km)
  %   RE - radius of Earth at location (km)
  %   S - scale (1/km)
  %   ZLB - lower bound altitude (km)
  %   TINF - temperature at infinite altitude (K)
  %   TA - Bates temperature at Z (K)
  % OUTPUT:
  %   DTA - derivative of Bates temperature with altitude (K/km)
  DTA = zeros(1,CIRA.DerLast);
  if length(RE) > 1
    a = RE(1,CIRA.Der0)+ZLB;
    if length(Z) > 1
      b = RE(1,CIRA.Der0)+Z(1,CIRA.Der0);
    else
      b = RE(1,CIRA.Der0)+Z;
    end
  else
    a = RE+ZLB;
    if length(Z) > 1
      b = RE+Z(1,CIRA.Der0);
    else
      b = RE+Z;
    end
  end
  f = (a/b)^2;
  fdRE = 2*(a/b)*(1-a/b)/b;
  %fdZ = 2*(a/b)*(-a/b)/b;
  if length(TINF) > 1 && length(TA) > 1
    DTA(1,CIRA.Der0) = (TINF(1,CIRA.Der0)-TA(1,CIRA.Der0))*S(1,CIRA.Der0)*f;
    for d=CIRA.Der0+1:CIRA.DerLast
      DTA(1,d) = ((TINF(1,d)-TA(1,d))*S(1,CIRA.Der0) ...
                 +(TINF(1,CIRA.Der0)-TA(1,CIRA.Der0))*S(1,d))*f ...
                 +(TINF(1,CIRA.Der0)-TA(1,CIRA.Der0))*S(1,CIRA.Der0)*fdRE*RE(1,d);
    end
  elseif length(TA) > 1
    DTA(1,CIRA.Der0)=(TINF-TA(1,CIRA.Der0))*S(1,CIRA.Der0)*f;
    for d=CIRA.Der0+1:CIRA.DerLast
      DTA(1,d) = ((-TA(1,d))*S(1,CIRA.Der0) ...
                +(TINF-TA(1,CIRA.Der0))*S(1,d))*f ...
                +(TINF-TA(1,CIRA.Der0))*S(1,CIRA.Der0)*fdRE*RE(1,d);
    end
  elseif length(TINF) > 1
    DTA(1,CIRA.Der0) = (TINF(1,CIRA.Der0)-TA)*S(1,CIRA.Der0)*f;
    for d=CIRA.Der0+1:CIRA.DerLast
      DTA(1,d) = (TINF(1,d)*S(1,CIRA.Der0) ...
                 +(TINF(1,CIRA.Der0)-TA)*S(1,d))*f ...
                 +(TINF(1,CIRA.Der0)-TA)*S(1,CIRA.Der0)*fdRE*RE(1,d);
    end
  end
end
