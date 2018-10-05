function [ lla, dlla ] = geodeticCoordinates( s, pos, vel )
%GEODETICCOORDINATES convert Cartesian coordinates to Geodetic coordinates
% This solution is the exact solution of the fourth order polynomial from
% <a href="matlab:web('http://www.astro.uni.torun.pl/~kb/Papers/geod/Geod-BG.htm')">Borkowski</a>
% with alterations for velocity and to calculate sin(lat) instead of tan(lat)
% INPUT:
% s - oblate spheroid model containing radius and eccentricity
% pos - array of Cartesian position X,Y,Z in meters
% vel - array of Cartesian velocity X,Y,Z in meters/second
% OUTPUT:
% lla - array of Geodetic latitude (radians), longitude (radians), altitude (m)
% dlla - array of Geodetic latitude rate (rad/s), longitude rate (rad/s), altitude rate (m/s)
  r_lim = 1.0e-8;
  a = s.equatorialRadius;
  b = s.sqrt1minusEcc2;
  b2 = b*b;
  
  [nr,nc] = size(pos);
  lla = zeros(nr,nc);
  if pos(3) >= 0.0
    sgn = 1;
  else
    sgn = -1;
  end
  
  x = double(pos(1))/a;
  y = double(pos(2))/a;
  z = sgn*double(pos(3))/a;
  
  r = hypot(x,y);
  
  % solve t^4 + 2*E*t^3 + 2*F*t - 1 = 0
  % solve u^3 + 4*(E*F+1)*u + 4*(E*E-F*F) = 0
  E = (b*z - (1 - b2))/2;
  F = (b*z + (1 - b2))/2;
  P = (4.0/3.0)*(E*F*4+r^2);
  %Q = 8*(E^2-F^2)*r;
  Qz = (8*b*(1 - b2))*z;
  Q = Qz*r;
  D = P^3 + Q^2;
  if D <= 0
    sP = sqrt(-P);
    theta = acos(-Q/(sP^3))/3;
    cosTheta = cos(theta);
    v = -sP*cosTheta/2;
    vz = v/r;
  elseif abs(r) < r_lim
    sD = sqrt(D);
    vz = Qz/sD/6;
    v = vz*r;
  else
    sD = sqrt(D);
    vp = (sD + Q)^(1.0/3.0);
    vm = (sD - Q)^(1.0/3.0);
    v = (vp - vm)/4;
    vz = v/r;
  end
  G = sqrt(E^2 + v*r) + E;
  %G = (-sqrt(E^2 + v*(r + 1)) + E)/2.0;
  t1z = (F-2*vz*G)/(G-E);
  if abs(r) > r_lim
    t = (sqrt(G^2 + t1z*r^2) - G)/r;
    tz = t/r;
  else
    tz = 0.5*t1z/G;
    t = tz*r;
  end
  %t = -sqrt(G^2 + (F*r-v*G)*r/(2*G-E)) - G;
  %tanLat = (r^2-t^2)/(2*b*t*r);
  u1 = 1.0-t^2;
  u2z = 2.0*b*tz;
  u2 = u2z*r;
  if abs(u2) < abs(u1)
    u = u2/u1; % [0,1)
    uz = u2z/u1; % [0,1)
    uu = sqrt(u*u+1.0); % [1,sqrt(2))
    sinLat = 1.0/uu; % (1/sqrt(2),1]
    %cosLat = u*sinLat; % (0,1]
    f = 1.0/(u*u+b2);
    N = b2*sqrt(f);
    alt = (z - N)*uu*a;
  else
    u = u1/u2; % [0,1]
    uu = sqrt(u*u+1.0); % [1,sqrt(2)]
    %cosLat = 1.0/uu; % [1/sqrt(2),1]
    sinLat = u/uu; % [0,1]
    f = 1.0/(1.0+b2*u*u);
    N = sqrt(f);
    alt = (r - N)*uu*a;
  end
  sinLat = sgn*sinLat;
  lla(1) = asin(sinLat);
  if r ~= 0
    lla(2) = atan2(y,x);
  else
    lla(2) = 0.0;
  end
  lla(3) = alt;

  if nargin > 2 && nargout == 2
    dlla = zeros(nr,nc);
    dx = double(vel(1))/a;
    dy = double(vel(2))/a;
    dz = sgn*double(vel(3))/a;

    dr = x*dx+y*dy;
    if r == 0
      drr = dx;
    else
      drr = dr / r;
    end

    % solve t^4 + 2*E*t^3 + 2*F*t - 1 = 0
    % solve u^3 + 4*(E*F+1)*u + 4*(E*E-F*F) = 0
    dE = (b*dz)/2;
    dF = (b*dz)/2;
    dP = (4.0/3.0)*(dE*F*4+E*dF*4+2*dr);
    %Q = 8*(E^2-F^2)*r;
    dQz = (8*b*(1 - b2))*dz;
    dQ = (8*b*(1 - b2))*dz*r^2+Qz*dr;
    dD = 3*P^2*dP + 2*Qz*dQ;
    if D <= 0
      dsP = -0.5*dP/sP;
      dtheta = -(-dQ+3.0*Q*r*dsP/sP)/(sP^3)/sqrt(1.0-(-Q/(sP^3))^2)/3;
      dv = -(dsP*cosTheta*r-sP*sin(theta)*dtheta)/2;
      dvz = (dv - vz*dr)/r;
    elseif abs(r) < r_lim
      dsD = 0.5*dD/sD;
      dvz = dQz/sD/6-Qz*dsD/sD/sD/6;
      %dv = dvz*r+vz*drr*r;
    else
      dsD = 0.5*dD/sD;
      dv = (1.0/3.0)*(vp*(dsD*r + dQ)/(sD + Q) - vm*(dsD*r - dQ)/(sD - Q))/4;
      dvz = (dv - vz*dr)/r;
    end
    dG = 0.5*(2*E*dE + dvz*r)/(G-E) + dE + vz*drr*r/(G-E);
    %G = (-sqrt(E^2 + v*r) + E)/2.0;
    dt1z = ((dF*r-2*dvz*G-2*vz*dG*r)-(F-2*vz*G)*(dG-dE)*r/(G-E))/(G-E);
    dt1 = dt1z+(F-2*vz*G)*drr/(G-E);
    if abs(r) > r_lim
      dt = 0.5*(2*G*dG/r + dt1)/(G + tz*r^2) - dG/r + drr*(0.5*t1z/(G + tz*r^2)-tz);
      %dt = 0.5*(2*G*dG + dt1z*(r + 1))/(G + tz*(r + 1)^2) - dG + dr*((0.5*t1z+(F-2*vz*G)/(G-E))/(G + tz*(r + 1)^2)-tz);
      drz = 0.5*(F-2*vz*G)/(G-E)/G;
      dtz = dt-drr*drz;
    else
      dtz = (0.5*dt1z-0.5*t1z*r*dG/G)/G;
      drz = 0.5*(F-2*vz*G)/(G-E)/G;
      %dt = dtz + drr*drz;
    end
    %t = -sqrt(G^2 + (F*r-v*G)*r/(2*G-E)) - G;
    %tanLat = (r^2-t^2)/(2*b*t*r);
    du1 = -2*tz;
    du2 = 2.0*b;
    if abs(u2) < abs(u1)
      duz = (du2-u2z*r^2*du1/u1)/u1;
      du = duz*dtz*r + dr*duz*drz;
      duu = uz*(duz*dtz*r + dr*duz*drz)/uu;
      dsinLat = -duz*(dtz + drr*drz)/uu^2;
      df = -2*uz*du*f*f;
      dN = b2*0.5*df/sqrt(f);
      dalt = ((dz - dN)*uu+(z - N)*duu)*a;
    else
      du = (du1-u1*du2/u2z/r^2)*(dtz + drr*drz)/u2z;
      duu = u*du/uu;
      dsinLat = du-u*duu/uu;
      df = -2.0*b2*u*du*f*f;
      dN = 0.5*df/N;
      dalt = ((drr - dN)*uu+(r - N)*duu)*a;
    end
    dlla(1) = sgn*dsinLat;
    if abs(r) > r_lim
      dlla(2) = (dy*x - y*dx)/(r^2);
    else
      dlla(2) = -dx;
      %dlla(2) = 0.0;
    end
    dlla(3) = dalt;
  end
  
end

