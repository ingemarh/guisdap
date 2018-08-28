function [ P1,L1 ] = CN2D( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NOP,NE,N2PLS,DISN2D,UVDISN,NPLUS,N2P,N2D,OPLS,NNO,N2A )
%CN2D CN2D
%
%.................... KEMPRN.FOR ......................................   
%.. This file contains the chemistry routines for ions and neutrals
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE CN2D(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NOP,NE,P1,L1
%     > ,N2PLS,DISN2D,UVDISN,NPLUS,N2P,N2D,OPLS,NNO,N2A)
%.......n(2d)

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);PR = zeros(22,1);
  PR(1)=NOP*NE*RTS(50);
  PR(2)=N2PLS*NE*RTS(32)*RTS(11);
  PR(3)=N2PLS*ON*RTS(10);
  PR(4)=DISN2D;
  PR(5)=RTS(63)*UVDISN;
  PR(6)=RTS(65)*NPLUS*O2N;
  PR(7)=N2P*RTS(57);
  PR(8)=RTS(27)*N2A*ON;
  LR(1)=ON*RTS(15);
  LR(2)=O2N*RTS(16);
  LR(3)=NE*RTS(8);
  LR(4)=OPLS*RTS(29);
  LR(5)=RTS(61);
  LR(6)=RTS(41)*NNO;
  P1=PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7)+PR(8);
  L1=LR(1)+LR(2)+LR(3)+LR(4)+LR(5)+LR(6);
%....... EF is used to convert production rates to volume emission rates
  EF=1.0;
%....... This line in for volume emission rates
%...      EF=RTS(61)*0.76/L1
  if JPT == 1 && JPR > 0 && floor(EF+0.1)~=1
    fprintf(I,'  N(2D)                          EMISSION                            :                    Loss rate');
  end
  if JPT == 1 && JPR > 0 && floor(EF+0.1) == 1
    fprintf(I,'  N(2D)                         Production                                    :                    Loss rate');
  end
  if JPT == 1 && JPR > 0
    fprintf(I,'ALT   [N2D]   NO++e   N2++e   N2++O    e+N2   hv+N2   N++O2   N(2P)   N2A+O    +O     +O2      +e     +O+     RAD     +NO');
  end
  if JPR > 0
    fprintf(I,'%6.1f,%8.1f', Z,P1/L1);
    for K=1:8
      fprintf(I,',%8.1f', PR(K)*EF);
    end
    for K=1:6
      fprintf(I,',%8.1f', LR(K)*N2D);
    end
  end


end

