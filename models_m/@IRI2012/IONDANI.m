function [ dion ] = IONDANI( id,ismo,hx,zd,fd,fs )
%IONDANI Danilov-Smirnova-1995 model and Danilov-Yaichnikov-1985 model (upper)
%
%        subroutine iondani(id,ismo,hx,zd,fd,fs,dion)
%-------------------------------------------------------
%       id      day of month
%       ismo    seasonal month (Northern Hemisphere January 
%                   is ismo=1 and so is Southern H. July)
%       hx      altitude in km
%       zd      solar zenith angle in degrees
%       fd      latitude in degrees
%       fs      10.7cm solar radio flux (12-month running mean)
%       dion(1)   O+  relative density in percent
%       dion(2)   H+  relative density in percent
%       dion(3)   N+  relative density in percent
%       dion(4)   He+ relative density in percent
%       dion(5)   NO+ relative density in percent
%       dion(6)   O2+ relative density in percent
%       dion(7)   Cluster+ relative density in percent
%
% Uses ionco2 (DS-95) for the molecular ions and ionco1 (DY-85)
% for the atomic ions.
%-------------------------------------------------------

%        dimension       dion(7)
%        common  /const/ umr

  h = hx;
  xhi = zd;
  xlati = fd;
  f107 = fs;
  deci_month = ismo + id/29.0;
  if h > 300.
    dion = IRI2012.IONCO1(h,xhi,xlati,f107,deci_month);
    dion(5)=0.0;
    dion(6)=0.0;
    dion(7)=0.0;
  else
    [rno,ro2,rcl,ro] = IRI2012.IONCO2(h,xhi,ismo,f107);
    dion = zeros(7,1);
    dion(1)=ro;
    dion(5)=rno;
    dion(6)=ro2;
    dion(7)=rcl;
  end

end

