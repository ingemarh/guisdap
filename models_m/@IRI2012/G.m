function [ funct ] = G( param,x )
%G vdrift calculations for G
%
%        subroutine g(param,funct,x)
%       *************************************************

%c        implicit real*8 (A-H,O-Z)
%        implicit real (A-H,O-Z)
%
%        integer i
%c        real*8 param(2),funct(6)
%c        real*8 x,a,sigma,gauss,flux,cflux
%        real param(2),funct(6)
%        real x,a,sigma,gauss,flux,cflux
%
%       *************************************************
  standardFlux = 140;
  funct = zeros(6,1);
  if(param(2) <= 75)
    flux=75.;
  elseif(param(2) >= 230)
    flux=230.;
  else
    flux=param(2);
  end

  if((param(1) >= 120) && (param(1) <= 360-120))
    a=170.;
    sigma=60;
  elseif((param(1) <= 60) || (param(1) >= 360-60))
    a=170.;
    sigma=40;
  else
    a=0.;
  end

  if((flux <= 95) && (a ~= 0))
    gauss=exp(-0.5*((x-a)^2)/sigma^2);
    cflux=gauss*95.+(1-gauss)*flux;
  else
    cflux=flux;
  end
  %       *************************************************

  %       *************************************************
  %for i=1:6
  %  funct(i)=0.;
  %end
  %       *************************************************
  p1lim = [45.0,75.0,105.0,135.0,230.0,260.0,290.0,320.0];

  if((param(1) >= p1lim(1)) && (param(1) <= p1lim(2)))  % W-E
    funct(1)=0;
    funct(2)=1.-(param(1)-p1lim(1))/(p1lim(2)-p1lim(1));
    funct(3)=1-funct(2);
  elseif((param(1) > p1lim(2)) && (param(1) < p1lim(3)))
    funct(1)=0;
    funct(2)=0;
    funct(3)=1;
  elseif((param(1) >= p1lim(3)) && (param(1) <= p1lim(4)))  % E-S
    funct(2)=0;
    funct(3)=1.-(param(1)-p1lim(3))/(p1lim(4)-p1lim(3));
    funct(1)=1-funct(3);
  elseif((param(1) > p1lim(4)) && (param(1) < p1lim(5)))
    funct(1)=1;
    funct(2)=0;
    funct(3)=0;
  elseif((param(1) >= p1lim(5)) && (param(1) <= p1lim(6)))  % S-E
    funct(1)=1.-(param(1)-p1lim(5))/(p1lim(6)-p1lim(5));
    funct(2)=0;
    funct(3)=1-funct(1);
  elseif((param(1) > p1lim(6)) && (param(1) < p1lim(7)))
    funct(1)=0;
    funct(2)=0;
    funct(3)=1;
  elseif((param(1) >= p1lim(7)) && (param(1) <= p1lim(8)))  % E-W
    funct(1)=0;
    funct(3)=1.-(param(1)-p1lim(7))/(p1lim(8)-p1lim(7));
    funct(2)=1-funct(3);
  else%if((param(1) <= p1lim(1)) || (param(1) >= p1lim(8)))
    funct(1)=0;
    funct(2)=1;
    funct(3)=0;
  end
  
  %       *************************************************
  funct(4)=(cflux-standardFlux)*funct(1);
  funct(5)=(cflux-standardFlux)*funct(2);
  funct(6)=(flux-standardFlux)*funct(3);
  %       *************************************************

end

