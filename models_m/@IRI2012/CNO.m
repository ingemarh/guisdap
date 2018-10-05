function [ P1,L1 ] = CNO( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,N2D,N4S,N2P,NNO,O2P,OPLS,PDNOSR,PLYNOP,N2A,NPLUS )
%CNO Summary of this function goes here
%   Detailed explanation goes here
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE CNO(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,P1,L1
%     > ,N2D,N4S,N2P,NNO,O2P,OPLS,PDNOSR,PLYNOP,N2A,NPLUS)
%........no
%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=RTS(16)*O2N*N2D;
  PR(2)=RTS(7)*O2N*N4S;
  PR(3)=RTS(38)*N2P*O2N;
  PR(4)=RTS(27)*N2A*ON;
  PR(5)=RTS(22)*NPLUS*O2N;          %.. Fox
  LR(1)=RTS(9)*N4S;
  LR(2)=RTS(23)*O2P;
  LR(3)=RTS(24)*OPLS;
  LR(4)=RTS(41)*N2D;
  LR(5)=PDNOSR;
  LR(6)=PLYNOP;
  P1=PR(1)+PR(2)+PR(3)+PR(4)+PR(5);
  L1=LR(1)+LR(2)+LR(3) + LR(4) + (LR(5)+LR(6));
%       IF(JPT.EQ.1.AND.JPR.GT.0) WRITE(I,192)
%  192  FORMAT(/2X,'NO',17X,'PRODUCTION',20X,':',10X,'LOSS RATES'/
%      > ,4X,'ALT',3X,'[NO]',5X,'[NO]c',3X,'O2+N2D',
%      > 3X,'O2+N4S   N2P+O2   N2A+O    N++O2    N4S+NO   O2P+NO   O++NO'
%      > ,3X,'N2D+NO   hv<1910   Lyman-a')
%       IF(JPR.GT.0) WRITE(I,7) Z,NNO,P1/L1,(PR(K),K=1,5)
%      > ,(LR(K)*NNO,K=1,6)
%       RETURN
%  7    FORMAT(F6.1,1P,22E9.2)
%       END


end

