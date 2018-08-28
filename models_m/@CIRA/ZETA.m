function [ Z, ZETAdRE, ZETAdZZ, ZETAdZL ] = ZETA( RE,ZZ,ZL )
%ZETA zeta function

  Z=(ZZ-ZL)*(RE+ZL)/(RE+ZZ);
  if nargout > 1
    ZETAdRE = (ZZ - ZL - Z)/(RE+ZZ);
    ZETAdZZ = (RE + ZL - Z)/(RE+ZZ);
    ZETAdZL = (ZZ - RE - 2*ZL)/(RE+ZZ);
  end

end

