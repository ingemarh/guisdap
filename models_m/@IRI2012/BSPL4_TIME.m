function [ bspl4_time ] = BSPL4_TIME( i,x1 )
%BSPL4_TIME bspl4_time
%        real function bspl4_time(i,x1)
%       *************************************************

%        implicit REAL*8 (A-H,O-Z)
%        implicit REAL (A-H,O-Z)
%		 
%        integer i,order,j,k
%c        real*8 t_t(0:39)
%c        real*8 x,b(20,20),x1
%        real t_t(0:39)
%        real x,b(20,20),x1
  persistent t_t;
  if isempty(t_t)
    t_t = [ ...
                0.00,2.75,4.75,5.50,6.25, ...
                7.25,10.00,14.00,17.25,18.00, ...
                18.75,19.75,21.00,24.00,26.75, ...
                28.75,29.50,30.25,31.25,34.00, ...
                38.00,41.25,42.00,42.75,43.75, ...
                45.00,48.00,50.75,52.75,53.50, ...
                54.25,55.25,58.00,62.00,65.25, ...
                66.00,66.75,67.75,69.00,72.00];
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
      b(k,j)=b(k,j)+(t_t(k+j+1)-x)/(t_t(k+j+1)-t_t(k+1+1))* ...
                b(k+1,j-1);
    end
  end

  bspl4_time=b(i,order);

end

