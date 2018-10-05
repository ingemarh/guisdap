function [ OP2P ] = COP2P( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD3,PSEC,HEPLUS,N4S,NNO,TE )
%COP2P COP2P
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE COP2P(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,OP2P,TPROD3,PSEC
%     > ,HEPLUS,N4S,NNO,TE)
%.......o+(2p)

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  PR = zeros(22,1);
  LR = zeros(22,1);
  PR(1)=0.0;
  if TPROD3 >= PSEC
    PR(1)=TPROD3-PSEC;
  end
  PR(2)=PSEC;
  PR(3)=HEPLUS*O2N*RTS(92);        %..Fox
  LR(1)=RTS(26)*ON;
  LR(2)=RTS(20)*N2N;
  LR(3)=RTS(13)*NE;
  LR(4)=0.218;
  LR(5)=RTS(14)*NE;
  LR(6)=(RTS(85)+RTS(86))*O2N;    %..Fox
  LR(7)=RTS(87)*N4S;              %..Fox
  LR(8)=RTS(88)*NNO;              %..Fox
  OP2P=(TPROD3+PR(3)) ...
    /(LR(1)+LR(2)+LR(3)+LR(4)+LR(5)+LR(6)+LR(7)+LR(8));
  if JPT == 1 && JPR > 0
    fprintf(I,'   O+(2P)      PRODUCTION          :            LOSS RATES   ALT   [O+2P]    hv+O     e*+O  He++O2      +O       +N2      +e       RAD      +e      +O2      +N4S     +NO      OX       N2        e      Te       E7320\n');
  end
  if JPR > 0
    for K=1:8
      fprintf(I,'%6.1f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f\n', Z,OP2P,PR(1),PR(2),PR(3),LR(K)*OP2P,ON,N2N,NE,TE,OP2P*0.218*0.781);
    end
  end

end

