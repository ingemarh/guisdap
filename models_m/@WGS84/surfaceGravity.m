function [ g ] = surfaceGravity( s, geodeticlatitude )
%SURFACEGRAVITY surface gravity for WGS84 model
%   WGS84 surface gravity
%   INPUT:
%   geodeticlatitude - geodetic latitude (radians)
%   OUTPUT:
%   g - gravitational acceleration (m/s^2)

  k = s.polarRadius*s.normalGravityAtPole/ ...
    (s.equatorialRadius*s.normalGravityAtEquator) - 1.0;
  
  sinLat = sin(double(geodeticlatitude));
  sinLat2 = sinLat*sinLat;
  g = s.normalGravityAtEquator*(1.0+k*sinLat2)/ ...
    sqrt(1.0-s.bodyEccentricity2*sinLat2);

end

