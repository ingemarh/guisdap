function [ j ] = LOCATE( xx,n,x )
%LOCATE binary search for value
%
%      SUBROUTINE locate(xx,n,x,j)
%-------------------------------------------------------------------------------

%      INTEGER j,n
%      REAL x,xx(n)
%      INTEGER jl,jm,ju
  jl=0;
  ju=n+1;
  while ju-jl > 1
    jm=floor((ju+jl)/2);
    if (xx(n) > xx(1)) == (x > xx(jm))
      jl=jm;
    else
      ju=jm;
    end
  end
  j=jl;

end

