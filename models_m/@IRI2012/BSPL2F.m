function [ bspl2f ] = BSPL2F( context, i,t1 )
%BSPL2F bspl2f
%*************************************************************************
%      function bspl2f(i,t1)
%*************************************************************************

%       dimension ts(0:9),b(30,30),
%     & ifnodes1(12),ifnodes2(12),ifnodes3(12)
%       common/mflux/kf,n
  persistent ifnodes1 ifnodes2 ifnodes3;
  if isempty(ifnodes1)
    ifnodes1 = [ 78, 77, 75, 79, 80, 77, 78, 80, 76, 81, 78, 78];
    ifnodes2 = [144,140,139,142,139,146,142,139,150,151,150,157];
    ifnodes3 = [214,211,201,208,213,220,203,209,213,215,236,221];
  end
  ts = zeros(10,1);
  b = zeros(30,30);

	ts(0+1)=ifnodes1(context.kf);
  ts(1+1)=ifnodes2(context.kf);
	ts(2+1)=ifnodes3(context.kf);
	ts(3+1)=ts(1+1)+367;
  ts(4+1)=ts(2+1)+367;
	ts(5+1)=ts(3+1)+367;
	ts(6+1)=ts(4+1)+367;
  ts(7+1)=ts(5+1)+367;
	ts(8+1)=ts(6+1)+367;
  ts(9+1)=ts(7+1)+367;

  t=t1;
  if(i >= 0 && t < ts(i+1))
     t=t+367.;
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

  bspl2f=b(i,2);

end

