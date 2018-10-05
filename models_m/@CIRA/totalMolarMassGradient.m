function dm = totalMolarMassGradient( context, D, incAnom )
  % total molar mass gradient calculation from previous call to GTD7 (g/mol/km)
  if nargin < 3
    incAnom = false;
  end
  totNumDens = CIRA.totalNumberDensity( D, incAnom );
  delDens = CIRA.totalNumberDensityGradient( D, incAnom );
  sm1 = 0.0;
  sm2 = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      sm1 = sm1 + context.MT(m)*D(CIRA.DensityIndex(CIRA.Der0,m));
      sm2 = sm2 + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerAlt,m));
    end
  end
  if nargin > 2 && incAnom == true
    sm1 = sm1 + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.Der0,CIRA.ANOM_O_MASS));
    sm2 = sm2 + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ANOM_O_MASS));
  end
  
  dm = sm2/totNumDens-sm1*delDens/totNumDens/totNumDens;
end
