function w = geostrophicWind( context, D, T )
  % calculate geostrophic wind approximation in EC/EF coordinates (m/s)
  % Horizontal motions are in balance with the pressure gradient force. 
  % Friction is negligible ( i.e., far away from surface). Steady flow with 
  % small curvature ( i.e., dV/dt = 0 ). Works well for z  1 km,  > 10°
  sinLat = sin(context.XL*CIRA.DGTR);
  cosLat = cos(context.XL*CIRA.DGTR);
  sinLon = sin(context.LONG*CIRA.DGTR);
  cosLon = cos(context.LONG*CIRA.DGTR);
  dP = context.totalPressure3DGradient( D, T );
  wgs = WGS84();
  lla = [context.XL*CIRA.DGTR,context.LONG*CIRA.DGTR,context.XALT*1000.0];
  dP(1) = dP(1)*100/CIRA.DGTR; % hPa/deg -> Pa/rad
  dP(2) = dP(2)*100/CIRA.DGTR; % hPa/deg -> Pa/rad
  dP(3) = dP(3)*100/1000.0; % hPa/km -> Pa/m
  dPecr = wgs.cartesianGradient(lla,dP); % Pa/m
  k = [cosLat * cosLon,  cosLat * sinLon, sinLat]; % vertical direction
  f = 2*context.SR*sinLat;
  w = (1/(f*D(CIRA.TOTAL_DENS)))*cross(k,dPecr); % Pa/m s (cm^3/g or m^3/kg)
  if context.IMR ~= 1
    w=w/CIRA.KGPERGM/CIRA.CM3PERM3; % cm^3/g -> m^3/kg
  end
end
