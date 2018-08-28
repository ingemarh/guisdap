function [ NPLUS ] = CNPLS( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,DISNP,OPLS,N2D,OP2P,HEPLUS,PHOTN,O2P,N4S,OP2D,N2PLS,NNO )
%CNPLS CNPLS
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE CNPLS(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,DISNP
%     > ,NPLUS,OPLS,N2D,OP2P,HEPLUS,PHOTN,O2P,N4S,OP2D,N2PLS,NNO)
%........n+

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=DISNP;
  PR(2)=RTS(29)*OPLS*N2D;
  PR(3)=0;
  PR(4)=RTS(45)*HEPLUS*N2N;
  PR(5)=PHOTN;
  PR(6)=O2P*N2D*RTS(78);          %..Fox
  PR(7)=N2PLS*N4S*RTS(79);        %..Fox
  PR(8)=OP2D*N4S*RTS(84);         %..Fox
  PR(9)=RTS(94)*NNO*HEPLUS;       %..Fox
  LR(1)=RTS(30)*O2N;              %..Fox
  LR(2)=RTS(25)*O2N;              %..Fox
  LR(3)=RTS(22)*O2N;              %..Fox
  LR(4)=RTS(65)*O2N;              %..Fox
  LR(5)=RTS(66)*O2N;              %..Fox
  LR(6)=RTS(31)*ON;               %..Fox

  CNPLUS=0.0;
  if LR(1)+LR(2)+LR(3) > 0.0
    CNPLUS=(PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7)+PR(8)+PR(9)) ...
      /(LR(1)+LR(2)+LR(3)+LR(4)+LR(5)+LR(6));
  end
  NPLUS=CNPLUS;

  if JPT == 1 && JPR > 0
    fprintf(I,'  N+                    PRODUCTION                                                                       :        LOSS RATES    ALT   [N+]   [N+]c     hv+N2   O++N2D  O+2P+N2   He++N2    hv+N   O2++N2D  N2++N4S O+(2D)+N4S  He++NO   N++O2    N++O2    N++O2    N++O2    N++O2    N++O\n');
  end
  if JPR > 0
    fprintf(I,'%6.1f,%9.2f,%9.2f', Z,NPLUS,CNPLUS);
    for K=1:9
      fprintf(I,',%9.2f', PR(K));
    end
    for K=1:6
      fprintf(I,',%9.2f', LR(K)*NPLUS);
    end
  end


end

