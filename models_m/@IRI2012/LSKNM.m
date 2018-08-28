function [ VAR,SING ] = LSKNM( N, M, M0, M1, HM, SC, HX, W,X,Y )
%LSKNM DETERMINES LAY-FUNCTIONS AMPLITUDES FOR A NUMBER OF CONSTRAINTS
%
%        SUBROUTINE LSKNM ( N, M, M0, M1, HM, SC, HX, W,X,Y,VAR,SING)
% --------------------------------------------------------------------
%   DETERMINES LAY-FUNCTIONS AMPLITUDES FOR A NUMBER OF CONSTRAINTS:
%
%       INPUT:  N       NUMBER OF AMPLITUDES ( LAY-FUNCTIONS)
%               M       NUMBER OF CONSTRAINTS
%               M0      NUMBER OF POINT CONSTRAINTS
%               M1      NUMBER OF FIRST DERIVATIVE CONSTRAINTS
%               HM      F PEAK ALTITUDE  [KM]
%               SC(N)   SCALE PARAMETERS FOR LAY-FUNCTIONS  [KM]
%               HX(N)   HEIGHT PARAMETERS FOR LAY-FUNCTIONS  [KM]
%               W(M)    WEIGHT OF CONSTRAINTS
%               X(M)    ALTITUDES FOR CONSTRAINTS  [KM]
%               Y(M)    LOG(DENSITY/NMF2) FOR CONSTRAINTS
%
%       OUTPUT: VAR(M)  AMPLITUDES
%               SING    =.TRUE.   NO SOLUTION
% ---------------------------------------------------------------------

%        LOGICAL         SING
%        DIMENSION       VAR(N), HX(N), SC(N), W(M), X(M), Y(M),
%     &                  BLI(5), ALI(5,5), XLI(5,10)
%
  BLI = zeros(N,1);
  ALI = zeros(N,N);
  XLI = zeros(N,M);
  VAR = zeros(N,1);
  M01=M0+M1;
  %SCM=0;
%   for J=1:5
%     BLI(J) = 0.;
%     for I=1:5
%       ALI(J,I) = 0.;
%     end
%   end
  for I=1:N
    for K=1:M0
      XLI(I,K) = IRI2012.RLAY( X(K), HM, SC(I), HX(I) );
    end
    for K=M0+1:M01
      XLI(I,K) = IRI2012.D1LAY( X(K), HM, SC(I), HX(I) );
    end
    for K=M01+1:M
      XLI(I,K) = IRI2012.D2LAY( X(K), HM, SC(I), HX(I) );
    end
  end
  for J=1:N
    for K=1:M
      BLI(J) = BLI(J) + W(K) * Y(K) * XLI(J,K);
      for I=1:N
        ALI(J,I) = ALI(J,I) + W(K) * XLI(I,K) ...
                * XLI(J,K);
      end
    end
  end
  [ALI, ~, SING] = IRI2012.LNGLSN( N, ALI, BLI );
  if ~SING
    for I=1:N
      VAR(I) = ALI(N,I);
    end
  end


end

