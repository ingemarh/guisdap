function [ P1,L1 ] = CN4S( JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,N4S,DISN4S,N2D,N2P,OPLS,N2PLS,UVDISN,NOP,NPLUS,NNO,O2P,PDNOSR,VCON )
%CN4S Summary of this function goes here
%   Detailed explanation goes here
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE CN4S(JPR,I,JPT,Z,RTS,ON,O2N,N2N,NE,P1,L1,N4S,DISN4S
%     >   ,N2D,N2P,OPLS,N2PLS,UVDISN,NOP,NPLUS,NNO,O2P,PDNOSR,VCON)
%........N(4S)

%      IMPLICIT REAL(A-H,L,N-Z)
%      DIMENSION RTS(99),LR(22),PR(22)
  LR = zeros(22,1);
  PR = zeros(22,1);
  PR(1)=DISN4S;
  PR(2)=RTS(15)*ON*N2D;
  PR(3)=RTS(8)*NE*N2D;
  PR(4)=VCON*RTS(3)*OPLS*N2N;
  if NE == 0 && N2PLS == Inf
    PR(5) = 0.0;
  else
    PR(5)=RTS(53)*RTS(11)*N2PLS*NE;
  end
  PR(6)=RTS(62)*UVDISN;
  PR(7)=NOP*NE*RTS(49);
  PR(8)=N2D*RTS(61);
  PR(9)=N2P*RTS(58);
  PR(10)=RTS(25)*NPLUS*O2N;
  PR(11)=PDNOSR*NNO;
  PR(12)=NPLUS*NNO*RTS(81);      %..Fox
  LR(1)=RTS(7)*O2N;
  LR(2)=RTS(9)*NNO;
  LR(3)=RTS(21)*O2P;
  LR(4)=RTS(79)*N2PLS;     %..Fox
  P1=PR(1)+PR(2)+PR(3)+PR(4)+PR(5)+PR(6)+PR(7)+PR(8)+PR(9)+PR(10) ...
       +PR(11)+PR(12);
  L1=LR(1)+LR(2)+LR(3)+LR(4);
  if JPT == 1 && JPR > 0
    fprintf(I,'  N(4S)                                      PRODUCTION                                              :       LOSS RATES\n');
%  193   FORMAT(/2X,'N(4S)',38X,'PRODUCTION',46X,':',7X,'LOSS RATES'/
%      > ,3X,'ALT',2X,'[N4S]',2X,'hv->N+'
%      > ,3X,'O+N2D',2X,'e+N2D',3X,'O++N2',3X,'N2++e',4X,'hv->2N'
%      > ,2X,'NO++e',2X,'N(2D)',4X,'N(2P)   N+&X    hv+NO    +O2  '
%      > ,2X,' +NO  ',2X,'+O2+ & N2+')
  end
  %PR(10)=PR(10)+PR(12);  %... for printing fit
  %LR(3)=LR(3)+LR(4);     %... for printing fit
  if JPR > 0
    %fprintf(I,'%6.1f,%8.1f', Z,N4S,(PR(K),K=1,11),(LR(K)*N4S,K=1,3);
  end

end

