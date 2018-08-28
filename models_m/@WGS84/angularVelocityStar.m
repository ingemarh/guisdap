function [ w ] = angularVelocityStar( JD )
%ANGULARVELOCITYSTAR angular velocity of the Earth in a precessing reference frame
% WGS84 3.2.4 eqn 3-10
% @param MJD Modified Julian Day in Universal Time (UT1)
% @return angular rotation rate (radians/second)
  jcn = 36525.0;
  j2k = 2451545.0;
  %jcn = IAU1976.JulianCentury;
  %j2k = IAU1976.J2000;

  du = floor(double(JD)) - j2k; % days since J2000.0
  Tu = du/jcn;
  m = 7.086e-12 + 4.3e-15*Tu;
  w = WGS84.angularVelocityPrime + m;

end

