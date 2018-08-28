function [ P1,O2P ] = CO2P( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD5,OPLS,OP2D,N2PLS,NPLUS,N4S,NNO,OP2P )
%CO2P CO2P
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE CO2P(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,P1
%     > ,O2P,TPROD5,OPLS,OP2D,N2PLS,NPLUS,N4S,NNO,OP2P)
%........o2+

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
%......... TPROD5=euv @ p.e. production
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=TPROD5;
  PR(2)=RTS(4)*O2N*OPLS;
  PR(3)=RTS(43)*OP2D*O2N;
  PR(4)=RTS(17)*O2N*N2PLS;
  PR(5)=RTS(25)*NPLUS*O2N;
  PR(6)=RTS(86)*OP2P*O2N;           %.. Fox
  PR(7)=RTS(65)*NPLUS*O2N;
  LR(1)=RTS(6)*NE;
  LR(2)=RTS(21)*N4S;
  LR(3)=RTS(23)*NNO;
  P1=PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7);
  if P1 == 0.0 % prevent 0/0
    O2P = 0.0;
  else
    O2P = P1/(LR(1)+LR(2)+LR(3));
  end
  if(JPT == 1 && JPR > 0)
    fprintf(I,'  O2+                          PRODUCTION                            :              LOSS RATES\n');
    fprintf(I,'   ALT   [O2+]   hv+O2   O++O2   O+(2D)+O2    N2++O2   N++O2   O+(2P)+O2  O2++e   O2++N   O2++NO\n');
  end
  if(JPR > 0)
    fprintf(I,'%6.1f %9.2e',Z,O2P);
    for K=1:7
      fprintf(I,' %9.2e',PR(K));
    end
    for K=1:3
      fprintf(I,' %9.2e',LR(K)*O2P);
    end
    fprintf(I,'\n');
  end

end

