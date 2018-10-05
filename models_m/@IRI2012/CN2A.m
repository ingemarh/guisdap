function [ N2A ] = CN2A( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,P3X1,P3X2,P3X3,P3X4 )
%CN2A CN2A
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE CN2A(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE
%     > ,N2A,P3X1,P3X2,P3X3,P3X4)
%........n2(a3sigma) and total LBH

%      IMPLICIT REAL(A-H,L,N-Z)
%      REAL P3X1,P3X2,P3X3,P3X4
%      DIMENSION RTS(99),LR(22),PR(22)
%....... pr(1,2,3)= electron impact excitation of n2(a,b,c) states
  LR = zeros(3,1);
  PR = zeros(3,1);
  PR(1)=P3X1;
  PR(2)=P3X2;
  PR(3)=P3X3;
  LR(1)=RTS(36)*ON;
  LR(2)=RTS(27)*ON;
  LR(3)=0.57;
  N2A=(PR(1)+PR(2)+PR(3))/(LR(1)+LR(2)+LR(3));
  if JPT == 1 && JPR > 0
    fprintf(I,'  N2(A)            PRODUCTION             :     LOSS RATES   :  Total LBH   ALT   N2(A)   e*->N2A   e*->N2B   e*->N2C  N2A>O1S  N2A>NO  RAD     LBH');
  end
  if JPR > 0
    fprintf(I,'%6.1f,%9.2f', Z,N2A);
    for K=1:3
      fprintf(I,',%9.2f', PR(K));
    end
    for K=1:3
      fprintf(I,',%9.2f', LR(K)*N2A);
    end
    fprintf(I,'%9.2f\n', P3X4);
  end

end

