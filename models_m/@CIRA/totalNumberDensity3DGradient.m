function delDens = totalNumberDensity3DGradient( D, incAnom )
  % total number density gradient calculation from previous call to GTD7 (1/cm^3/km or 1/m^3/km)
  densdAlt = 0.0;
  densdLon = 0.0;
  densdLat = 0.0;
  for m = CIRA.HE_MASS:CIRA.N_MASS
    if m ~= CIRA.HOT_O_MASS
      densdAlt = densdAlt + D(CIRA.DensityIndex(CIRA.DerAlt,m));
      densdLon = densdLon + D(CIRA.DensityIndex(CIRA.DerLon,m));
      densdLat = densdLat + D(CIRA.DensityIndex(CIRA.DerLat,m));
    end
  end
  if nargin > 1 && incAnom == true
    densdAlt = densdAlt + D(CIRA.DensityIndex(CIRA.DerAlt,CIRA.ANOM_O_MASS));
    densdLon = densdLon + D(CIRA.DensityIndex(CIRA.DerLon,CIRA.ANOM_O_MASS));
    densdLat = densdLat + D(CIRA.DensityIndex(CIRA.DerLat,CIRA.ANOM_O_MASS));
  end
  delDens = [densdLat,densdLon,densdAlt];
end
