function [ bspl4_long ] = BSPL4_LONG( i,x1 )
%BSPL4_LONG bspl4_long
%
%        real function bspl4_long(i,x1)
%c        real*8 function bspl4_long(i,x1)
%       *************************************************

%c       implicit real*8 (A-H,O-Z) 
%       implicit real (A-H,O-Z) 
%
%        integer i,order,j,k
%c        real*8 t_l(0:24)
%c        real*8 x,b(20,20),x1
%        real t_l(0:24)
%        real x,b(20,20),x1
  persistent t_l;
  if isempty(t_l)
    t_l = [ ...
                0,10,100,190,200,250,280,310, ...
                360,370,460,550,560,610,640,670, ...
                720,730,820,910,920,970,1000,1030,1080];
  end
  b = zeros(20,20);
  order=4;
  x=x1;
  if(i >= 0)
    if (x < t_l(i-0+1))
        x=x+360;
    end
  end
  for j=i:i+order-1
     if(x >= t_l(j+1) && x < t_l(j+1+1))
        b(j,1)=1;
     else
        b(j,1)=0;
     end
  end

  for j=2:order
    for k=i:i+order-j
       b(k,j)=(x-t_l(k+1))/(t_l(k+j-1+1)-t_l(k+1))*b(k,j-1);
       b(k,j)=b(k,j)+(t_l(k+j+1)-x)/(t_l(k+j+1)-t_l(k+1+1))* ...
                 b(k+1,j-1);
    end
  end

  bspl4_long=b(i,order);


end

