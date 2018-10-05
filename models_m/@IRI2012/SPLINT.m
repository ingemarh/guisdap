function [ y ] = SPLINT( xa,ya,y2a,n,x )
%SPLINT splint
%
%      SUBROUTINE splint(xa,ya,y2a,n,x,y)
%-------------------------------------------------------------------------------

%      INTEGER n
%      REAL x,y,xa(n),y2a(n),ya(n)
%      INTEGER k,khi,klo
%      REAL a,b,h
  klo=1;
  khi=n;
  while khi-klo > 1
    k=floor((khi+klo)/2);
    if xa(k) > x
      khi=k;
    else
      klo=k;
    end
  end
  h=xa(khi)-xa(klo);
  if h == 0
    a=(xa(khi)-x);
    b=(x-xa(klo));
    y=a*ya(klo)+b*ya(khi)+((a^3-a*h*h)*y2a(klo)+(b^3-b*h*h)*y2a(khi))/6.;
    if y == 0
      y = 1; % indeterminant 0/0
    else
      y = y / h; % +/- infinity
    end
  else
    a=(xa(khi)-x)/h;
    b=(x-xa(klo))/h;
    y=a*ya(klo)+b*ya(khi)+((a^3-a)*y2a(klo)+(b^3-b)*y2a(khi))*(h^ ...
     2)/6.;
  end

end

