function [ g ] = centrifugalGravity( position, velocity, w, dw )
%CENTRIFUGALGRAVITY centrifugal effect on acceleration due to rotation
% input:
% position - ECI position (meters)
% velocity - ECI velocity (meters/second)
% w angular rotation (radians/second)
% output:
% g = -w x (w x r) + 2*w x v (meters/second^2)
% ECI = R * ECEF
% R = exp(Omega)
% dR = Omega*R
% ddR = dOmega*R + Omega*Omega*R
% r = R*r'
% v = R*v' + dR*r'
% a = R*a' + 2*dR*v' + ddR*r'
% a = R*a' + 2*Omega*v + dOmega*r - Omega*Omega*r
  if nargin < 4
    dw = 0.0;
  end
  if nargin < 3
    w = velocity;
    velocity = zeros(1,3);
  end
  w2 = w*w;
  g(1) = w2*position(1) - dw*position(2) - 2*w*velocity(2);
  g(2) = w2*position(2) + dw*position(1) + 2*w*velocity(1);
  g(3) = 0.0;

end

