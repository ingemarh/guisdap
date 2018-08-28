function totNumDens = totalNumberDensity( D, incAnom )
  % total number density calculation from previous call to GTD7 (1/cm^3 or 1/m^3)
  totNumDens = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      totNumDens = totNumDens + D(CIRA.DensityIndex(CIRA.Der0,m));
    end
  end
  if nargin > 1 && incAnom == true
    totNumDens = totNumDens + D(CIRA.DensityIndex(CIRA.Der0,CIRA.ANOM_O_MASS));
  end
end
