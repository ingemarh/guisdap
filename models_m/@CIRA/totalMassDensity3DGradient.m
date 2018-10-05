function delDens = totalMassDensity3DGradient( context, D, incAnom )
  % total mass density gradient calculation from previous call to GTD7 (g/cm^3/km or kg/m^3/km)
  delDensdAlt = 0.0;
  delDensdLon = 0.0;
  delDensdLat = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      delDensdAlt = delDensdAlt + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerAlt,m));
      delDensdLon = delDensdLon + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerLon,m));
      delDensdLat = delDensdLat + context.MT(m)*D(CIRA.DensityIndex(CIRA.DerLat,m));
    end
  end
  if nargin > 2 && incAnom == true
    delDensdLat = delDensdLat + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerLat,CIRA.ANOM_O_MASS));
    delDensdLon = delDensdLon + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerLon,CIRA.ANOM_O_MASS));
    delDensdAlt = delDensdAlt + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ANOM_O_MASS));
  end
  delDens = CIRA.INVAVOG*[delDensdLat,delDensdLon,delDensdAlt];
  if context.IMR == 1
    delDens=delDens*CIRA.KGPERGM; % g/m^3 -> kg/m^3
  end
end
