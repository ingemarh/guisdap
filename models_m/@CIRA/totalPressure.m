function P = totalPressure( context, D, T, incAnom )
  % total pressure calculation from previous call to GTD7 (hPa)
  if nargin < 4
    incAnom = false;
  end
  XN = CIRA.totalNumberDensity( D, incAnom );
  P = CIRA.BM*XN*T(CIRA.TemperatureIndex(CIRA.Der0)); % hPa * cm^3 * (cm^-3 or m^-3)
  if context.IMR == 1
    P=P/CIRA.CM3PERM3; % m^3 -> cm^3
  end
end
