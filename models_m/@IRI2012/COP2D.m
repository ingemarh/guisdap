function [ OP2D ] = COP2D( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,TPROD2,OP2P,HEPLUS,N4S,NNO,PSEC )
%COP2D COP2D
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE COP2D(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,OP2D,TPROD2,OP2P
%     > ,HEPLUS,N4S,NNO,PSEC)
%.......o+(2d)

%      IMPLICIT REAL (A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=TPROD2;                % EUV  prod
  PR(2)=OP2P*NE*RTS(13);
  PR(3)=OP2P*0.171;
  PR(4)=HEPLUS*O2N*RTS(76);     %..Fox
	PR(5)=PSEC;
  LR(1)=RTS(19)*N2N;
  LR(2)=7.7E-5;                 %.. radiation at 3726 and 3729 A
  LR(3)=NE*RTS(12);
  LR(4)=ON*RTS(28);
  LR(5)=RTS(43)*O2N;
  LR(6)=RTS(83)*NNO;      %..Fox
  LR(7)=RTS(84)*N4S;      %..Fox
  LR(8)=RTS(90)*N2N;      %.. -> NO+ + N, Li et al. [1997] 
  OP2D=(PR(1)+PR(2)+PR(3)+PR(4)+PR(5))/ ...
     (LR(1)+LR(2)+LR(3)+LR(4)+LR(5)+LR(6)+LR(7)+LR(8));
  if JPT == 1 && JPR > 0
    fprintf(I,'  O+(2D)             PRODUCTION                           :                  LOSS RATES   ALT   [O+2D]   hv+O    e*+O    O+2P+e   O+2P>hv  He++O2     +N2    E3726_29    +e       +O      +O2      +NO     +N  +N2>NO+');
  end
  if JPR > 0
    for K=1:8
      fprintf(I,'%6.1f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f,%9.2f', ...
        Z,OP2D,PR(1),PR(5),PR(2),PR(3),PR(4),LR(K)*OP2D);
    end
  end

end

