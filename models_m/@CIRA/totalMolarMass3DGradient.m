function dm = totalMolarMass3DGradient( context, D, incAnom )
  % total molar mass gradient calculation from previous call to GTD7 (g/mol/km)
  if nargin < 3
    incAnom = false;
  end
  totNumDens = CIRA.totalNumberDensity( D, incAnom );
  delDens = CIRA.totalNumberDensity3DGradient( D, incAnom );
  sm = 0.0;
  smdLat = 0.0;
  smdLon = 0.0;
  smdAlt = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      sm = sm + context.MT(m)*D(CIRA.DensityIndex(CIRA.Der0,m));
      smdLat = smdLat + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerLat,m));
      smdLon = smdLon + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerLon,m));
      smdAlt = smdAlt + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerAlt,m));
    end
  end
  if nargin > 2 && incAnom == true
    sm = sm + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.Der0,CIRA.ANOM_O_MASS));
    smdLat = smdLat + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerLat,CIRA.ANOM_O_MASS));
    smdLon = smdLon + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerLon,CIRA.ANOM_O_MASS));
    smdAlt = smdAlt + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ANOM_O_MASS));
  end
  dm = delDens*(-sm/totNumDens/totNumDens);
  dm(1) = dm(1) + smdLat/totNumDens;
  dm(2) = dm(2) + smdLon/totNumDens;
  dm(3) = dm(3) + smdAlt/totNumDens;
end
