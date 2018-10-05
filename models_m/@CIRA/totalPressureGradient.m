function dP = totalPressureGradient( context, D, T, incAnom )
  % total pressure gradient calculation from previous call to GTD7 (hPa/km)
  if nargin < 4
    incAnom = false;
  end
  XN = CIRA.totalNumberDensity( D, incAnom );
  dXN = CIRA.totalNumberDensityGradient( D, incAnom );
  dP = CIRA.BM*dXN*T(CIRA.TemperatureIndex(CIRA.Der0)) ...
    + CIRA.BM*XN*T(CIRA.TemperatureIndex(CIRA.DerAlt)); % hPa/km * cm^3 * (cm^-3 or m^-3)
  if context.IMR == 1
    dP=dP/CIRA.CM3PERM3; % m^3 -> cm^3
  end
end
