function [ N2PLS ] = CN2PLS( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,PUN2PX,PUN2PA,PUN2PB,PEN2PX,PEN2PA,PEN2PB,OP2D,OP2P,HEPLUS,NPLUS,NNO,N4S )
%CN2PLS Summary of this function goes here
%   Detailed explanation goes here
%::::::::::::::::::::::::::::::: CN2PLS :::::::::::::::::::::::::::::::
%..... Simplified chemistry of N2+.  PUN2P* = production of N2+ by euv 
%..... in the (X,A,B states). PEN2P* same for p.e.s (X,A,B states)
%      SUBROUTINE CN2PLS(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,N2PLS,PUN2PX,
%     >  PUN2PA,PUN2PB,PEN2PX,PEN2PA,PEN2PB,OP2D,OP2P,HEPLUS,NPLUS,
%     >  NNO,N4S)
%      IMPLICIT REAL(A-H,L,N-Z)
%      REAL PUN2PX,PUN2PA,PUN2PB,PEN2PX,PEN2PA,PEN2PB
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);PR = zeros(22,1);
  PR(1)=PUN2PX;
  PR(2)=PUN2PA;
  PR(3)=PUN2PB;
  PR(4)=PEN2PX;
  PR(5)=PEN2PA;
  PR(6)=PEN2PB;
  PR(7)=RTS(19)*OP2D*N2N;
  PR(8)=RTS(20)*OP2P*N2N;
  PR(9)=RTS(44)*HEPLUS*N2N;
  PR(10)=RTS(82)*NPLUS*NNO;  %..Fox
  LR(1)=RTS(10)*ON;
  LR(2)=RTS(11)*NE;
  LR(3)=RTS(17)*O2N;
  LR(4)=RTS(99)*ON;
  LR(5)=RTS(79)*N4S;     %..Fox
  LR(6)=RTS(80)*NNO;     %..Fox
  if (LR(1)+LR(2)+LR(3)+LR(4)+LR(5)+LR(6)) == 0.0
    N2PLS = 0.0;
  else
    N2PLS= ...
      (PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7)+PR(8)+PR(9)+PR(10))/ ...
      (LR(1)+LR(2)+LR(3)+LR(4)+LR(5)+LR(6));
  end
  if JPT == 1 && JPR > 0
    fprintf(I,'N2+                             PRODUCTION                                             :            LOSS RATES   ALT  [N2+]  EUV-X   EUV-A    EUV-B   PE-X     PE-A    PE-B  O+2D+N2  O+2P+N2  He++N2  O+N2+  e+N2+  O2+N2+  N2++O  Other');
  end
  PR(9)=PR(9)+PR(10);     %.. for printing fit
  LR(5)=LR(5)+LR(6);      %.. for printing fit
  if JPR > 0
    fprintf(I,'%6.1f,%8.1f', Z,N2PLS);
    for K=1:9
      fprintf(I,',%8.1f', PR(K));
    end
    for K=1:5
      fprintf(I,',%8.1f', LR(K)*N2PLS);
    end
  end


end

