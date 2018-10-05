function tm = totalMolarMass( context, D, incAnom )
  % total molar mass calculation from previous call to GTD7 (g/mol)
  if nargin < 3
    incAnom = false;
  end
  totNumDens = CIRA.totalNumberDensity( D, incAnom );
  sm = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      sm = sm + context.MT(m)*D(CIRA.DensityIndex(CIRA.Der0,m));
    end
  end
  if nargin > 2 && incAnom == true
    sm = sm + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.Der0,CIRA.ANOM_O_MASS));
  end
  tm = sm/totNumDens;
end
