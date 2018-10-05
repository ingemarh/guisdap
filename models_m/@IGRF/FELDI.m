function [ H, BABS, BEAST, BNORTH, BDOWN ] = FELDI( context, RQ, XXX, YYY, ZZZ, ...
  IS, CP, SP, CT, ST )
%FELDI ENTRY POINT  FELDI  USED FOR L COMPUTATION
%*****ENTRY POINT  FELDI  USED FOR L COMPUTATION                        

%      ENTRY FELDI                                                       
  %IS=3;                                                              
  H = zeros(IGRF.numCoeff,1);
  IHMAX=context.NMAX*context.NMAX+1;                                                 
  LAST=IHMAX+context.NMAX+context.NMAX;                                              
  IMAX=context.NMAX+context.NMAX-1;                                                  
  for I=IHMAX:LAST                                                 
    H(I)=context.GH1(I);
  end
  for K=1:2:3
    I=IMAX;
    IH=IHMAX;
    while I >= K
      IL=IH-I;
      F=2./double(I-K+2);
      X=XXX*RQ*F;
      Y=YYY*RQ*F;
      Z=ZZZ*RQ*(F+F);
      I=I-2;
      if I > 1 % I-1 > 0
        for M=3:2:I                                                      
          H(IL+M+1)=context.GH1(IL+M+1)+Z*H(IH+M+1)+X*(H(IH+M+3)-H(IH+M-1))-Y*(H(IH+M+2)+H(IH+M-2));
          H(IL+M)=context.GH1(IL+M)+Z*H(IH+M)+X*(H(IH+M+2)-H(IH+M-2))+Y*(H(IH+M+3)+H(IH+M-1));
        end
      end
      if I > 0 % I-1 == 0
        H(IL+2)=context.GH1(IL+2)+Z*H(IH+2)+X*H(IH+4)-Y*(H(IH+3)+H(IH));
        H(IL+1)=context.GH1(IL+1)+Z*H(IH+1)+Y*H(IH+4)+X*(H(IH+3)-H(IH));
      end
      % I-1 < 0
      H(IL)=context.GH1(IL)+Z*H(IH)+2.*(X*H(IH+1)+Y*H(IH+2));
      IH=IL;
    end
  end

  if nargout == 1 || IS == 3
    return;
  end

  S=.5*H(1)+2.*(H(2)*ZZZ...
               +H(3)*XXX...
               +H(4)*YYY)*RQ;
  T=(RQ+RQ)*sqrt(RQ);
  BXXX=T*(H(3)-S*XXX);
  BYYY=T*(H(4)-S*YYY);
  BZZZ=T*(H(2)-S*ZZZ);

  if IS~= 2
    BABS=sqrt(BXXX*BXXX+BYYY*BYYY+BZZZ*BZZZ);
    BEAST = BYYY*CP-BXXX*SP;
    BRHO  = BYYY*SP+BXXX*CP;
    BNORTH= BZZZ*ST-BRHO*CT;
    BDOWN =-BZZZ*CT-BRHO*ST;
  else
    B = IGRF.newVector();
    B(1)=BXXX;
    B(2)=BYYY;
    B(3)=BZZZ;
    BABS = B;
  end

end

