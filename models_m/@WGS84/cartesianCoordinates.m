function [ pos, vel ] = cartesianCoordinates( s, lla, dlla )
%CARTESIANCOORDINATES convert Geodetic coordinates to Cartesian coordinates
% INPUT:
% s - oblate spheroid model containing radius and eccentricity
% lla - array of Geodetic latitude (radians), longitude (radians), altitude (m)
% dlla - array of Geodetic latitude rate (rad/s), longitude rate (rad/s), altitude rate (m/s)
% OUTPUT:
% pos - array of Cartesian position X,Y,Z in meters
% vel - array of Cartesian velocity X,Y,Z in meters/second
  lat = lla(1);
  lon = lla(2);
  alt = lla(3);
  pos = lla;
  vel = pos;
  vel(1) = 0.0;
  vel(2) = 0.0;
  vel(3) = 0.0;
  sinLat = sin(lat);
  cosLat = cos(lat);
  sinLon = sin(lon);
  cosLon = cos(lon);
  f = 1.0/(1.0-s.bodyEccentricity2*sinLat*sinLat);
  N = s.equatorialRadius*sqrt(f);
  NcL = (N+alt)*cosLat;
  z2 = s.oneMinusEcc2*N+alt;
  x = NcL*cosLon;
  y = NcL*sinLon;
  z = z2*sinLat;
  pos(1) = x;
  pos(2) = y;
  pos(3) = z;
  if nargin > 2 && nargout > 1
    NsL = (N+alt)*sinLat;
    dN = N*s.bodyEccentricity2*sinLat*cosLat*f*dlla(1);
    dNcL = (dN+dlla(3))*cosLat-NsL*dlla(1);
    dz2 = s.oneMinusEcc2*dN+dlla(3);
    dx = dNcL*cosLon-NcL*sinLon*dlla(2);
    dy = dNcL*sinLon+NcL*cosLon*dlla(2);
    dz = dz2*sinLat+z2*cosLat*dlla(1);
    vel(1) = dx;
    vel(2) = dy;
    vel(3) = dz;
  end

end

