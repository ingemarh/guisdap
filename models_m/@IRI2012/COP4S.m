function [ OPLS ] = COP4S( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD1,OP2D,OP2P,PEPION,PDISOP,N2PLS,N2D,NNO,VCON,HEPLUS )
%COP4S COP4S
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE COP4S(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,OPLS,TPROD1,OP2D
%     >  ,OP2P,PEPION,PDISOP,N2PLS,N2D,NNO,VCON,HEPLUS)
%...........o+(4s)

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
%.........pr(1)= euv production of o+(4s)
  NPLUS = 0.0;
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=TPROD1;
  PR(2)=OP2D*NE*RTS(12);
  PR(3)=OP2P*ON*RTS(26);
  PR(4)=PEPION;
  PR(5)=PDISOP;
  PR(6)=RTS(99)*N2PLS*ON;
  PR(7)=OP2P*NE*RTS(14);
  PR(8)=OP2P*0.047;
  PR(9)=RTS(28)*ON*OP2D;
  PR(10)=RTS(85)*OP2P*O2N;              %.. Fox
  PR(11)=HEPLUS*O2N*(RTS(91)+RTS(93));  %..Fox
  PR(12)=RTS(95)*NNO*HEPLUS;            %..Fox
  PR(13)=RTS(22)*NPLUS*O2N;             %..Fox
  LR(1)=N2N*VCON*RTS(3);
  LR(2)=O2N*RTS(4);
  LR(3)=NNO*RTS(24);
  LR(4)=N2D*RTS(29);    %.... small loss?? ..Fox
  %..LR(4)=(LR(1)+LR(2)+LR(3))  !.. total loss for printing
  PR(10)=PR(10)+PR(11)+PR(12)+PR(13);
  PRTOT=PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7)+PR(8)+PR(9)+PR(10);
  LRTOT=LR(1)+LR(2)+LR(3)+LR(4);
  OPLS=PRTOT/LRTOT;
  if JPT == 1 && JPR > 0
    fprintf(I,'  O+                                         PRODUCTION                                       :         LOSS RATES ALT    [O+]   hv+O  O+(2D)+e O+(2P)+O   e*+O  O2-diss  N2++O  O+(2P)+e O+(2P) O+O+(2D)   Other  +N2     +O2    +NO   +N2D');
  end
  if JPR > 0
    fprintf(I,'%6.1f,%8.1f', Z,OPLS);
    for K=1:10
      fprintf(I,',%8.1f', PR(K));
    end
    for K=1:4
      fprintf(I,',%8.1f', LR(K)*OPLS);
    end
  end

end

