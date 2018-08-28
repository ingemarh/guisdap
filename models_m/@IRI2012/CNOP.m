function [ P1,NOP ] = CNOP( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,OPLS,N2PLS,O2P,N4S,NNO,NPLUS,N2P,PLYNOP,VCON,N2D,OP2D )
%CNOP CNOP
%:::::::::::::::::::::::::::::: CNOP ::::::::::::::::::::::::::::::::::
%      SUBROUTINE CNOP(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,P1,NOP,OPLS
%     >  ,N2PLS,O2P,N4S,NNO,NPLUS,N2P,PLYNOP,VCON,N2D,OP2D)
%........no+

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=VCON*RTS(3)*N2N*OPLS;
  PR(2)=N2PLS*ON*RTS(10);
  PR(3)=O2P*N4S*RTS(21);
  PR(4)=O2P*NNO*RTS(23);
  %.. N+ + O2 -> O2+ + N(2D,4S) or NO+ + O(1S)
  PR(5)=(RTS(30)+RTS(66)+RTS(59))*NPLUS*O2N;
  PR(6)=RTS(37)*N2P*ON;
  PR(7)=RTS(24)*OPLS*NNO;
  PR(8)=PLYNOP*NNO;
  PR(9)=O2P*N2D*RTS(77);         %..Fox
  PR(10)=N2PLS*NNO*RTS(80);      %..Fox
  PR(11)=NPLUS*NNO*RTS(81);      %..Fox
  PR(12)=RTS(83)*NNO*OP2D;       %..Fox
  PR(13)=OP2D*RTS(90)*N2N;        %.. -> NO+ + N, Li et al. [1997] 
  LR(1)=NE*RTS(5);
  P1=PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7)+PR(8) ...
      +PR(9)+PR(10)+PR(11)+PR(12)+PR(13);
  if LR(1) == 0.0
    NOP = 0.0;
  else
    NOP=P1/LR(1);
  end

%       IF(JPT.EQ.1.AND.JPR.GT.0) WRITE(I,96)
%  96   FORMAT(/2X,'NO+',31X,'PRODUCTION',48X,':',2X,'LOSS RATES'/
%      > ,3X,'ALT',3X,'[NO+]',4X,'O++N2',3X,'N2++O',3X,'O2++N4S'
%      > ,3X,'O2++NO',3X,'N++O2',4X,'N2P+O',3X,'O++NO   hv+NO'
%      > ,5X,'O2++N2D   N2++NO   N++NO   OP2D+NO   OP2D+N2  NO++e')
%       %PR(9)=PR(9)+PR(10)+PR(11)+PR(12)+PR(13)
%       IF(JPR.GT.0) WRITE(I,7) Z,NOP,(PR(K),K=1,13),LR(1)*NOP
%       RETURN
%  7    FORMAT(F6.1,1P,22E9.2)
%       END


end

