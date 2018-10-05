function [ A ] = SWAPEL( N,A )
%SWAPEL swaps elements of array
%
%       SUBROUTINE SWAPEL(N,A)
%-------------------------------------------------------------------------------
%      swaps elements of array
%-------------------------------------------------------------------------------

%       INTEGER N,I  
%       REAL A(N),AT(N)
  AT = A;
  for I=0:N-1
    A(I+1)=AT(N-I);
  end

end

