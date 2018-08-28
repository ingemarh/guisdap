function [ YI, YIdX, YIdXA, YIdYA, YIdYD1, YIdYD2 ] = SPLINI( XA,YA,Y2A,M,N,C,X,...
  Y2AdXA,Y2AdYA,Y2AdYD1,Y2AdYD2 )
%SPLINI INTEGRATE CUBIC SPLINE FUNCTION FROM XA(1) TO X
%      SUBROUTINE SPLINI(XA,YA,Y2A,N,X,YI)
%-----------------------------------------------------------------------
%       INTEGRATE CUBIC SPLINE FUNCTION FROM XA(1) TO X
%        XA,YA: ARRAYS OF TABULATED FUNCTION IN ASCENDING ORDER BY X
%        Y2A: ARRAY OF SECOND DERIVATIVES
%        N: SIZE OF ARRAYS XA,YA,Y2A
%        X: ABSCISSA ENDPOINT FOR INTEGRATION
%        Y: OUTPUT VALUE
%-----------------------------------------------------------------------

%      DIMENSION XA(N),YA(N),Y2A(N)
%      SAVE
  num = N-M+1;
  if nargin < 6
    Y2AdXA = zeros(num,num);
    Y2AdYA = zeros(num,num);
    Y2AdYD1 = zeros(num,1);
    Y2AdYD2 = zeros(num,1);
  end
  YI=0;
  YIdX=0;
  YIdXA = zeros(num,1);
  YIdYA = zeros(num,1);
  YIdYD1=0;
  YIdYD2=0;
  for KLO=M:N-1
    if X <= XA(KLO,C)
      break;
    end
    KHI=KLO+1;
    H=XA(KHI,C)-XA(KLO,C);
    if KHI < N && X > XA(KHI,C)
      A=0.0;
      B=H;
      AdX = 0.0;
      BdX = 0.0;
      AdXAHI = 0.0;
      BdXAHI = 1.0;
    else
      A=XA(KHI,C)-X;
      B=X-XA(KLO,C);
      AdX = -1.0;
      BdX = 1.0;
      AdXAHI = 1.0;
      BdXAHI = 0.0;
    end
    A2=A*A;
    B2=B*B;
    A2dX=2*A*AdX;
    B2dX=2*B*BdX;
    f1 = (-(H^4+A2*A2)/4.+A2*H*H/2.)/6.;
    f1dX = A2dX*(-(2*A2)/4.+H*H/2.)/6.;
    f2 = (B2*B2/4.-B2*H*H/2.)/6.;
    f2dX = B2dX*(2*B2/4.-H*H/2.)/6.;
    nYI = (H*H-A2)*YA(KLO,C)/2.+B2*YA(KHI,C)/2. ...
          +f1*Y2A(KLO-M+1)+f2*Y2A(KHI-M+1);
    nYIdX = -A2dX*YA(KLO,C)/2.+B2dX*YA(KHI,C)/2. ...
          +f1dX*Y2A(KLO-M+1)+f2dX*Y2A(KHI-M+1);
    nYIdYD1 = f1*Y2AdYD1(KLO-M+1)+f2*Y2AdYD1(KHI-M+1);
    nYIdYD2 = f1*Y2AdYD2(KLO-M+1)+f2*Y2AdYD2(KHI-M+1);
    for K=1:num
      nYIdXA = f1*Y2AdXA(KLO-M+1,K)+f2*Y2AdXA(KHI-M+1,K);
      nYIdYA = f1*Y2AdYA(KLO-M+1,K)+f2*Y2AdYA(KHI-M+1,K);
      if KLO-M+1 == K
        HdXA = -1.0;
        AdXA = 0.0;
        BdXA = -1.0;
        A2dXA=2*A*AdXA;
        B2dXA=2*B*BdXA;
        f1dXA = (-(4*H^3*HdXA+2*A2*A2dXA)/4.+A2dXA*H*H/2.+2*A2*H*HdXA/2.)/6.;
        f2dXA = (2*B2*B2dXA/4.-B2dXA*H*H/2.-2*B2*H*HdXA/2.)/6.;
        nYIdXA = nYIdXA + (2*H*HdXA-A2dXA)*YA(KLO,C)/2.+B2dXA*YA(KHI,C)/2. ...
          +f1dXA*Y2A(KLO-M+1)+f2dXA*Y2A(KHI-M+1)-nYI*HdXA/H;
        nYIdYA = nYIdYA + (H*H-A2)/2.;
      elseif KHI-M+1 == K
        HdXA = 1.0;
        A2dXA=2*A*AdXAHI;
        B2dXA=2*B*BdXAHI;
        f1dXA = (-(4*H^3*HdXA+2*A2*A2dXA)/4.+A2dXA*H*H/2.+2*A2*H*HdXA/2.)/6.;
        f2dXA = (2*B2*B2dXA/4.-B2dXA*H*H/2.-2*B2*H*HdXA/2.)/6.;
        nYIdXA = nYIdXA + (2*H*HdXA-A2dXA)*YA(KLO,C)/2.+B2dXA*YA(KHI,C)/2. ...
          +f1dXA*Y2A(KLO-M+1)+f2dXA*Y2A(KHI-M+1)-nYI*HdXA/H;
        nYIdYA = nYIdYA + B2/2.;
      end
      YIdYA(K)=YIdYA(K)+nYIdYA/H;
      YIdXA(K)=YIdXA(K)+nYIdXA/H;
    end
    if H == 0
      % leave YI alone since it will cause NaNs eventually
      fprintf(1,'BAD XA INPUT TO SPLINI KLO %d KHI %d\n',KLO,KHI);
    else
      YI = YI + nYI/H;
      YIdX = YIdX + nYIdX/H;
      YIdYD1 = YIdYD1 + nYIdYD1/H;
      YIdYD2 = YIdYD2 + nYIdYD2/H;
    end
  end
end

