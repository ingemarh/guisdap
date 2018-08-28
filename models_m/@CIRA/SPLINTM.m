function [ Y,YdX,YdXA,YdYA,YdYD1,YdYD2 ] = SPLINTM( XA,YA,Y2A,M,N,C,X,...
  Y2AdXA,Y2AdYA,Y2AdYD1,Y2AdYD2 )
%SPLINTM CALCULATE CUBIC SPLINE INTERP VALUE
%      SUBROUTINE SPLINTM(XA,YA,Y2A,N,X,Y)
%-----------------------------------------------------------------------
%        CALCULATE CUBIC SPLINE INTERP VALUE
%        ADAPTED FROM NUMERICAL RECIPES BY PRESS ET AL.
%        XA,YA: ARRAYS OF TABULATED FUNCTION IN ASCENDING ORDER BY X
%        Y2A: ARRAY OF SECOND DERIVATIVES
%        N: SIZE OF ARRAYS XA,YA,Y2A
%        X: ABSCISSA FOR INTERPOLATION
%        Y: OUTPUT VALUE
%        YdX: DERIVATIVE OF OUTPUT VALUE w.r.t. X
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
  YdYA = zeros(num,1);
  YdXA = zeros(num,1);
  
  [KLO,KHI] = BinarySearch(X,XA,M,N,C);
  H=XA(KHI,C)-XA(KLO,C);
  if H == 0
    fprintf(1,'BAD XA INPUT TO SPLINTM KLO=%d KHI=%d\n',KLO,KHI);
    AH=XA(KHI,C)-X;
    BH=X-XA(KLO,C);
    AH3 = (AH*AH-H*H)*AH/6.;
    BH3 = (BH*BH-H*H)*BH/6.;
    Y=AH*YA(KLO,C)+BH*YA(KHI,C)+AH3*Y2A(KLO-M+1)+BH3*Y2A(KHI-M+1);
    AHdX=-1.0;
    BHdX=1.0;
    AH3dX = (3.0*AH*AH-H*H)*AHdX/6.;
    BH3dX = (3.0*BH*BH-H*H)*BHdX/6.;
    YdX=AHdX*YA(KLO,C)+BHdX*YA(KHI,C)+AH3dX*Y2A(KLO-M+1)+BH3dX*Y2A(KHI-M+1);
    if Y ~= 0
      Y = Y/H; % +/- infinity
      YdX = YdX/H; % +/- infinity
      for K=1:num
        YdXA(K) = 0.0;
        YdYA(K) = YdX;
      end
      YdYD1 = YdX;
      YdYD2 = YdX;
    else
      Y = 0.0; % indeterminate 0/0
      YdX = 0.0; % indeterminate 0/0
      for K=1:num
        YdXA(K) = 0.0;
        YdYA(K) = 0.0;
      end
      YdYD1 = 0.0;
      YdYD2 = 0.0;
    end
  else
    A = (XA(KHI,C)-X)/H;
    B = (X-XA(KLO,C))/H;
    A3 = (A*A*A-A)*H*H/6.;
    B3 = (B*B*B-B)*H*H/6.;
    Y = A*YA(KLO,C)+B*YA(KHI,C)+A3*Y2A(KLO-M+1)+B3*Y2A(KHI-M+1);
    AdX = -1.0/H;
    BdX = 1.0/H;
    A3dA = (3*A*A-1)*H*H/6.;
    B3dB = (3*B*B-1)*H*H/6.;
    YdX=AdX*YA(KLO,C)+BdX*YA(KHI,C)+A3dA*AdX*Y2A(KLO-M+1)+B3dB*BdX*Y2A(KHI-M+1);
    YdYD1 = A3*Y2AdYD1(KLO)+B3*Y2AdYD1(KHI-M+1);
    YdYD2 = A3*Y2AdYD2(KLO)+B3*Y2AdYD2(KHI-M+1);
    for K=1:num
      YdYA(K) = A3*Y2AdYA(KLO-M+1,K)+B3*Y2AdYA(KHI-M+1,K);
      YdXA(K) = A3*Y2AdXA(KLO-M+1,K)+B3*Y2AdXA(KHI-M+1,K);
      if KLO-M+1 == K
        YdYA(K) = YdYA(K) + A;
        HdXA = -1.0;
        AdXA =      - A*HdXA/H;
        BdXA = -1/H - B*HdXA/H;
        A3dXA = A3dA*AdXA+A3*2*HdXA/H;
        B3dXA = B3dB*BdXA+B3*2*HdXA/H;
        YdXA(K) = YdXA(K) + AdXA*YA(KLO,C)+BdXA*YA(KHI,C)+A3dXA*Y2A(KLO-M+1)+B3dXA*Y2A(KHI-M+1);
      elseif KHI-M+1 == K
        YdYA(K) = YdYA(K) + B;
        HdXA = 1.0;
        AdXA =  1/H - A*HdXA/H;
        BdXA =      - B*HdXA/H;
        A3dXA = A3dA*AdXA+A3*2*HdXA/H;
        B3dXA = B3dB*BdXA+B3*2*HdXA/H;
        YdXA(K) = YdXA(K) + AdXA*YA(KLO,C)+BdXA*YA(KHI,C)+A3dXA*Y2A(KLO-M+1)+B3dXA*Y2A(KHI-M+1);
      end
    end
  end
end

function [KLO,KHI] = BinarySearch(X,XA,M,N,C)
  KLO=M;
  KHI=N;
  while KHI-KLO > 1
    K=floor((KHI+KLO)/2);
    if XA(K,C) > X
      KHI=K;
    else
      KLO=K;
    end
  end
end
