function totMassDens = totalMassDensity( context, D, incAnom )
  % total mass density calculation from previous call to GTD7 (g/cm^3 or kg/m^3)
  totMassDens = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      totMassDens = totMassDens + context.MT(m)*D(CIRA.DensityIndex(CIRA.Der0,m));
    end
  end
  if nargin > 2 && incAnom == true
    totMassDens = totMassDens + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.Der0,CIRA.ANOM_O_MASS));
  end
  totMassDens = CIRA.INVAVOG*totMassDens;
  if context.IMR == 1
    totMassDens=totMassDens*CIRA.KGPERGM; % g/m^3 -> kg/m^3
  end
end
