function [ bspl4_ptime ] = BSPL4_PTIME( i,x1 )
%BSPL4_PTIME Summary of this function goes here
%   Detailed explanation goes here
%      real function bspl4_ptime(i,x1)
%       real*8 function bspl4_ptime(i,x1)
% *************************************************

%c       IMPLICIT REAL*8 (A-H,O-Z)
%       IMPLICIT REAL (A-H,O-Z)

%       integer i,order,j,k
%c       real*8 t_t(0:27)
%c       real*8 x,b(20,20),x1
%       real t_t(0:27)
%       real x,b(20,20),x1
  persistent t_t;
  if isempty(t_t)
    t_t = [0.00,3.00,4.50,6.00,9.00,12.0,15.0,18.0,21.0, ...
                24.0,27.0,28.5,30.0,33.0,36.0,39.0,42.0,45.0, ...
                48.0,51.0,52.5,54.0,57.0,60.0,63.0,66.0,69.0,72.0];
  end
  b = zeros(20,20);
  order=4;
  x=x1;
  if(i >= 0)
    if (x < t_t(i-0+1))
      x=x+24;
    end
  end
  for j=i:i+order-1
    if(x >= t_t(j+1) && x < t_t(j+1+1))
      b(j,1)=1;
    else
      b(j,1)=0;
    end
  end

  for j=2:order
    for k=i:i+order-j
      b(k,j)=(x-t_t(k+1))/(t_t(k+j-1+1)-t_t(k+1))*b(k,j-1);
      b(k,j)=b(k,j)+(t_t(k+j+1)-x)/(t_t(k+j+1)-t_t(k+1+1))*b(k+1,j-1);
    end
  end
  bspl4_ptime=b(i,order);

end

