function delDens = totalNumberDensityGradient( D, incAnom )
  % total number density gradient calculation from previous call to GTD7 (1/cm^3/km or 1/m^3/km)
  delDens = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      delDens = delDens + D(CIRA.DensityIndex(CIRA.DerAlt,m));
    end
  end
  if nargin > 1 && incAnom == true
    delDens = delDens + D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ANOM_O_MASS));
  end
end
