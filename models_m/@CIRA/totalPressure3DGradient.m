function dP = totalPressure3DGradient( context, D, T, incAnom )
  % total pressure gradient calculation from previous call to GTD7 (hPa/km)
  if nargin < 4
    incAnom = false;
  end
  XN = CIRA.totalNumberDensity( D, incAnom );
  dXN = CIRA.totalNumberDensity3DGradient( D, incAnom );
  dP = CIRA.BM*dXN*T(CIRA.TemperatureIndex(CIRA.Der0)); % hPa/km * cm^3 * (cm^-3 or m^-3)
  dP(3) = dP(3) + CIRA.BM*XN*T(CIRA.TemperatureIndex(CIRA.DerAlt)); % hPa/km * cm^3 * (cm^-3 or m^-3)
  dP(2) = dP(2) + CIRA.BM*XN*T(CIRA.TemperatureIndex(CIRA.DerLon)); % hPa/deg * cm^3 * (cm^-3 or m^-3)
  dP(1) = dP(1) + CIRA.BM*XN*T(CIRA.TemperatureIndex(CIRA.DerLat)); % hPa/deg * cm^3 * (cm^-3 or m^-3)
  if context.IMR == 1
    dP=dP/CIRA.CM3PERM3; % m^3 -> cm^3
  end
end
