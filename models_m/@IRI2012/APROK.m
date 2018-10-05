function [ R1,R2 ] = APROK( j1m,j2m,h1,h2,R1m,R2m,rk1m,rk2m,hei,xhi )
%APROK aprok
%
%      Subroutine aprok(j1m,j2m,h1,h2,R1m,R2m,rk1m,rk2m,hei,xhi,R1,R2)
%----------------------------------------------------------------- 

%      dimension   zm(7),j1m(7),j2m(7),h1(13,7),h2(13,7),R1m(13,7),
%     *            R2m(13,7),rk1m(13,7),rk2m(13,7)
  persistent zm;
  if isempty(zm)
    zm = [20,40,60,70,80,85,90];
  end
      
  h=hei;
  z=xhi;

  j1=1;
  j2=1;
  i1=1;
  for i=1:length(zm)
    i1=i;
    if z == zm(i)
      j1=0;
    end
    if z <= zm(i)
      break;
    end
  end
  while true
    i2=1;
    for i=2:j1m(i1)
      i2=i-1;
      if h < h1(i,i1)
        break;
      end
      i2=j1m(i1);
    end
    i3=1;
    for i=2:j2m(i1)
      i3=i-1;
      if h < h2(i,i1)
        break;
      end
      i3=j2m(i1);
    end
    R01=R1m(i2,i1);
    R02=R2m(i3,i1);
    rk1=rk1m(i2,i1);
    rk2=rk2m(i3,i1);
    h01=h1(i2,i1);
    h02=h2(i3,i1);
    R1=R01+rk1*(h-h01);
    R2=R02+rk2*(h-h02);
    if j1 == 1
      j1=0;
      j2=0;
      i1=i1-1;
      R11=R1;
      R12=R2;
      continue;
    elseif j2 == 0
      rk=(z-zm(i1))/(zm(i1+1)-zm(i1));
      R1=R1+(R11-R1)*rk;
      R2=R2+(R12-R2)*rk;
    end
    break;
  end

end

