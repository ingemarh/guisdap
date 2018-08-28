function delDens = totalMassDensityGradient( context, D, incAnom )
  % total mass density gradient calculation from previous call to GTD7 (g/cm^3/km or kg/m^3/km)
  delDens = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      delDens = delDens + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerAlt,m));
    end
  end
  if nargin > 2 && incAnom == true
    delDens = delDens + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ANOM_O_MASS));
  end
  delDens = CIRA.INVAVOG*delDens;
  if context.IMR == 1
    delDens=delDens*CIRA.KGPERGM; % g/m^3 -> kg/m^3
  end
end
