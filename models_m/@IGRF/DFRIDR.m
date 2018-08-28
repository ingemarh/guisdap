function [ DFRIDR,err ] = DFRIDR( context,func,x,h )
%DFRIDR Derivative of func at x with initial interval h
%
%      FUNCTION DFRIDR(func,x,h,err)
% **********************************************************************
%  Numerical Recipes Fortran 77 Version 2.07
%  Copyright (c) 1986-1995 by Numerical Recipes Software
% **********************************************************************

%        COMMON/iounit/konsol        
%
%      INTEGER NTAB
%      REAL dfridr,err,h,x,func,CON,CON2,BIG,SAFE
%      PARAMETER (CON=1.4,CON2=CON*CON,BIG=1.E30,NTAB=10,SAFE=2.)
%      EXTERNAL func
%
%      INTEGER i,j
%      REAL errt,fac,hh,a(NTAB,NTAB)
  persistent a CON CON2 BIG NTAB SAFE;
  if isempty(a)
    CON = 1.4;
    CON2 = CON*CON;
    BIG = 1.E30;
    NTAB = 10;
    SAFE = 2.;
    a = zeros(NTAB,NTAB);
  end
  if h == 0.
    if context.KONSOL > 0
      fprintf(context.KONSOL,'h (%f) must be nonzero in DFRIDR\n',h);
    end
    DFRIDR = 0.0; % indeterminant 0/0
    err = BIG;
    return;
  end
  hh = h;
  a(1,1) = (func(context,x+hh)-func(context,x-hh))/(2.0*hh);
  err = BIG;
  for i=2:NTAB
    hh = hh/CON;
    a(1,i) = (func(context,x+hh)-func(context,x-hh))/(2.0*hh);
    fac = CON2;
    for j=2:i
      a(j,i) = (a(j-1,i)*fac-a(j-1,i-1))/(fac-1.);
      fac = CON2*fac;
      errt = max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)));
      if errt <= err
        err = errt;
        DFRIDR = a(j,i);
      end
    end
    if abs(a(i,i)-a(i-1,i-1)) >= SAFE*err
      return;
    end
  end

end

