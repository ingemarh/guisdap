function [ A,B,AUS ] = LNGLSN( N, A, B )
%LNGLSN SOLVES QUADRATIC SYSTEM OF LINEAR EQUATIONS
%
%        SUBROUTINE LNGLSN ( N, A, B, AUS)
% --------------------------------------------------------------------
% SOLVES QUADRATIC SYSTEM OF LINEAR EQUATIONS:
%
%       INPUT:  N       NUMBER OF EQUATIONS (= NUMBER OF UNKNOWNS)
%               A(N,N)  MATRIX (LEFT SIDE OF SYSTEM OF EQUATIONS)
%               B(N)    VECTOR (RIGHT SIDE OF SYSTEM)
%
%       OUTPUT: AUS     =.TRUE.   NO SOLUTION FOUND
%                       =.FALSE.  SOLUTION IS IN  A(N,J) FOR J=1,N
% --------------------------------------------------------------------

%        DIMENSION       A(5,5), B(5), AZV(10)
%        LOGICAL         AUS
%
  AZV = zeros(N+1,1);
  %NN = N - 1;
  AUS = false;
  for K=1:N-1
    IMAX = K;
    L    = K;
    IZG  = 0;
    AMAX  = abs( A(K,K) );
    while true
      L = L + 1;
      if L > N
        break;
      end
      HSP = abs( A(L,K) );
      if HSP < 1.E-8
        IZG = IZG + 1;
      end
      if HSP > AMAX
        break;
      end
    end
    if abs(AMAX) < 1.E-10
      AUS = true;
      return;
    end
    if IMAX ~= K
      for L=K:N
        AZV(L+1)  = A(IMAX,L);
        A(IMAX,L) = A(K,L);
        A(K,L)    = AZV(L+1);
      end
      AZV(1)  = B(IMAX);
      B(IMAX) = B(K);
      B(K)    = AZV(1);
    end
    if IZG ~= (N-K)
      AMAX = 1. / A(K,K);
      AZV(1) = B(K) * AMAX;
      for M=K+1:N
        AZV(M+1) = A(K,M) * AMAX;
      end
      for L=K+1:N
        AMAX = A(L,K);
        if abs(AMAX) < 1.E-8
          continue;
        end
        A(L,K) = 0.0;
        B(L) = B(L) - AZV(1) * AMAX;
        for M=K+1:N
          A(L,M) = A(L,M) - AMAX * AZV(M+1);
        end
      end
    end
  end
  for K=N:-1:1
    AMAX = 0.0;
    if K < N
      for L=K+1:N
        AMAX = AMAX + A(K,L) * A(N,L);
      end
    end
    if abs(A(K,K)) < 1.E-6
      A(N,K) = 0.0;
    else
      A(N,K) = ( B(K) - AMAX ) / A(K,K);
    end
  end

end

