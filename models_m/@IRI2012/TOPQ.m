function [ TOPQ, dTOPQ ] = TOPQ( h,No,hmax,Ho )
%TOPQ NeQuick formula
%        REAL FUNCTION TOPQ(h,No,hmax,Ho)
%----------------------------------------------------------------
%  NeQuick formula
%----------------------------------------------------------------

%        REAL No
  persistent g rfac;
  if isempty(g)
    g=0.125;
    rfac=100.0;
  end
  dh=h-hmax;
  ddh=1.0;
  g1=g*dh;
  dg1=g*ddh;
  z=dh/(Ho*(1.0+rfac*g1/(rfac*Ho+g1)));
  dz=ddh/(Ho*(1.0+rfac*g1/(rfac*Ho+g1))) ...
    - z*(Ho*(rfac*dg1/(rfac*Ho+g1)-rfac*g1*dg1/(rfac*Ho+g1)/(rfac*Ho+g1))) ...
    /(Ho*(1.0+rfac*g1/(rfac*Ho+g1)));
  if z > 40
    TOPQ=0.0;
    dTOPQ=0.0;
  else
    ee=exp(z);
    dee=ee*dz;
    if ee > 1.0e7
      ep=4.0/ee;
      dep=-4.0*dee/ee/ee;
    else
      ep=4.0*ee/(1.0+ee)^2;
      dep=4.0*dee/(1.0+ee)^2 - 2*4.0*ee*dee/(1.0+ee)^3;
    end
    TOPQ=No*ep;
    dTOPQ=No*dep;
  end
end

