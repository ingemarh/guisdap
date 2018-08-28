function [ bspl2l ] = BSPL2L( i,t1 )
%BSPL2L bspl2l
%*******************************************************************
%      function bspl2l(i,t1)
%*******************************************************************

%      dimension ts(0:6),b(30,30)
  persistent ts;
  if isempty(ts)
    ts = [ 94.,112.5,454.,472.5,814.,832.5,1174.];
  end
  b = zeros(30,30);
  
  t=t1;
  if(i >= 0 && t < ts(i+1))
     t=t+360.;
  end
  for j=i:i+2-1
    if(t >= ts(j+1) && t < ts(j+1+1))
       b(j,1)=1.;
    else
       b(j,1)=0.;
    end
  end

  for j=2:2
    for k=i:i+2-j
      b(k,j)=(t-ts(k+1))/(ts(k+j-1+1)-ts(k+1))*b(k,j-1);
      b(k,j)=b(k,j)+(ts(k+j+1)-t)/(ts(k+j+1)-ts(k+1+1))*b(k+1,j-1);
    end
  end

  bspl2l=b(i,2);

end

