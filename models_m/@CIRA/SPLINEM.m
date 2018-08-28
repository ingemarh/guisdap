function [ Y2,Y2dX,Y2dY,Y2dYP1,Y2dYPN ] = SPLINEM( X,Y,M,N,C,YP1,YPN )
%SPLINEM CALCULATE 2ND DERIVATIVES OF CUBIC SPLINE INTERP FUNCTION
%      SUBROUTINE SPLINEM(X,Y,N,YP1,YPN,Y2)
%-----------------------------------------------------------------------
%        CALCULATE 2ND DERIVATIVES OF CUBIC SPLINE INTERP FUNCTION
%        ADAPTED FROM NUMERICAL RECIPES BY PRESS ET AL
%        X,Y: ARRAYS OF TABULATED FUNCTION IN ASCENDING ORDER BY X
%        N: SIZE OF ARRAYS X,Y
%        YP1,YPN: SPECIFIED DERIVATIVES AT X(1) AND X(N); VALUES
%                 >= 1E30 SIGNAL SIGNAL SECOND DERIVATIVE ZERO
%        Y2: OUTPUT ARRAY OF SECOND DERIVATIVES
%        Y2dY: OUTPUT MATRIX OF DERIVATIVE OF SECOND DERIVATIVES w.r.t. dY2(i)/dY(j)
%        Y2dYP1: OUTPUT ARRAY OF DERIVATIVE OF SECOND DERIVATIVES w.r.t. dY2(i)/dYP1
%        Y2dYPN: OUTPUT ARRAY OF DERIVATIVE OF SECOND DERIVATIVES w.r.t. dY2(i)/dYPN
%-----------------------------------------------------------------------

%      PARAMETER(NMAX=100)
%      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
%      SAVE
  persistent U UdX UdY UdYP1 YPMAX;
  if isempty(U)
    NMAX = 100;
    U = zeros(NMAX,1);
    UdY = zeros(NMAX,NMAX);
    UdX = zeros(NMAX,NMAX);
    UdYP1 = zeros(NMAX,1);
    YPMAX = 0.99E30;
  end
  num = N-M+1;
  Y2 = zeros(num,1);
  Y2dX = zeros(num,num);
  Y2dY = zeros(num,num);
  Y2dYP1 = zeros(num,1);
  Y2dYPN = zeros(num,1);
  if nargin < 4 || YP1 > YPMAX
    Y2(1)=0;
    for I=1:num
      Y2dX(1,I)=0;
      Y2dY(1,I)=0;
      UdX(1,I)=0;
      UdY(1,I)=0;
    end
    Y2dYP1(1)=0;
    Y2dYPN(1)=0;
    U(1)=0;
    UdYP1(1)=0;
  else
    Y2(1)=-0.5;
    for I=1:num
      Y2dX(1,I) = 0;
      Y2dY(1,I) = 0;
      UdX(1,I) = 0;
      UdY(1,I) = 0;
    end
    Y2dYP1(1) = 0;
    Y2dYPN(1) = 0;
    f = 1/(X(M+1,C)-X(M,C));
    fd1 = f*f;
    fd2 = -fd1;
    U(1)=3*f*((Y(M+1,C)-Y(M,C))*f-YP1);
    UdX(1,1)=3*fd1*(2*(Y(M+1,C)-Y(M,C))*f-YP1);
    UdX(1,2)=3*fd2*(2*(Y(M+1,C)-Y(M,C))*f-YP1);
    UdY(1,1)=-3*f*f;
    UdY(1,2)=3*f*f;
    UdYP1(1)=-3*f;
  end
  for K=2:(num-1)
    f0=1./(X(K+1+M-1,C)-X(K-1+M-1,C));
    f0dn=f0*f0;
    f0dp=-f0dn;
    fp=1./(X(K+1+M-1,C)-X(K+M-1,C));
    fpd0=fp*fp;
    fpdp=-fpd0;
    fn=1./(X(K+M-1,C)-X(K-1+M-1,C));
    fnd0=-fn*fn;
    fndn=-fnd0;
    SIG=f0/fn;
    P=SIG*Y2(K-1)+2.;
    Y2(K)=(SIG-1.)/P;
    U(K)=(6.*((Y(K+1+M-1,C)-Y(K+M-1,C))*fp-(Y(K+M-1,C)-Y(K-1+M-1,C))*fn)*f0-SIG*U(K-1))/P;
    Y2dYP1(K)=-Y2(K)*SIG*Y2dYP1(K-1)/P;
    Y2dYPN(K)=-Y2(K)*SIG*Y2dYPN(K-1)/P;
    UdYP1(K) = (-SIG*UdYP1(K-1) - U(K)*SIG*Y2dYP1(K-1))/P;
    for I=1:num
      Y2dY(K,I)=-Y2(K)*SIG*Y2dY(K-1,I)/P;
      if K-1 == I
        SIGdX = f0dn/fn-SIG*fndn/fn;
        UdX(K,I)=-6.*(Y(K+M-1,C)-Y(K-1+M-1,C))*f0*fndn...
          +6.*((Y(K+1+M-1,C)-Y(K+M-1,C))*fp-(Y(K+M-1,C)-Y(K-1+M-1,C))*fn)*f0dn;
        UdY(K,I) = 6.*fn*f0;
      elseif K == I
        SIGdX =        -SIG*fnd0/fn;
        UdX(K,I)=6.*((Y(K+1+M-1,C)-Y(K+M-1,C))*fpd0-(Y(K+M-1,C)-Y(K-1+M-1,C))*fnd0)*f0;
        UdY(K,I) = -6.*(fp+fn)*f0;
      elseif K+1 == I
        SIGdX = f0dp/fn;
        UdX(K,I)=6.*(Y(K+1+M-1,C)-Y(K+M-1,C))*f0*fpdp...
          +6.*((Y(K+1+M-1,C)-Y(K+M-1,C))*fp-(Y(K+M-1,C)-Y(K-1+M-1,C))*fn)*f0dp;
        UdY(K,I) = 6.*fp*f0;
      else
        SIGdX = 0.0;
        UdX(K,I) = 0.0;
        UdY(K,I) = 0.0;
      end
      Y2dX(K,I) = (SIGdX - Y2(K)*SIGdX*Y2(K-1) - Y2(K)*SIG*Y2dX(K-1,I))/P;
      UdX(K,I) = (UdX(K,I) - SIG*UdX(K-1,I) - U(K)*SIG*Y2dX(K-1,I) ...
        - SIGdX*U(K-1) - U(K)*SIGdX*Y2(K-1))/P;
      UdY(K,I) = (UdY(K,I) - SIG*UdY(K-1,I) - U(K)*SIG*Y2dY(K-1,I))/P;
    end
  end
  if nargin < 5 || YPN > YPMAX
    QN = 0;
    UN = 0;
    UNdXN = 0.0;
    UNdXN1 = 0.0;
    UNdYPN = 0.0;
    UNdYN = 0.0;
    UNdYN1 = 0.0;
  else
    QN = 0.5;
    f = 1./(X(N,C)-X(N-1,C));
    fdXN = -f*f;
    fdXN1 = f*f;
    UN = 3*f*(YPN-(Y(N,C)-Y(N-1,C))*f);
    UNdXN = 3*fdXN*(YPN-2*(Y(N,C)-Y(N-1,C))*f);
    UNdXN1 = 3*fdXN1*(YPN-2*(Y(N,C)-Y(N-1,C))*f);
    UNdYPN = 3*f;
    UNdYN = -3*f*f;
    UNdYN1 = 3*f*f;
  end
  f = 1/(QN*Y2(num-1)+1.);
  Y2(num)=(UN-QN*U(num-1))*f;
  for I=1:num
    if I == num
      Y2dX(num,I)=UNdXN;
      Y2dY(num,I)=UNdYN;
    elseif I == num-1
      Y2dX(num,I)=UNdXN1;
      Y2dY(num,I)=UNdYN1;
    else
      Y2dX(num,I)=0.0;
      Y2dY(num,I)=0.0;
    end
    Y2dX(num,I) = (Y2dX(num,I)-QN*UdX(num-1,I) - Y2(num)*QN*Y2dX(num-1,I))*f;
    Y2dY(num,I) = (Y2dY(num,I)-QN*UdY(num-1,I) - Y2(num)*QN*Y2dY(num-1,I))*f;
  end
  Y2dYP1(num)=(-QN*UdYP1(num-1) - Y2(num)*QN*Y2dYP1(num-1))*f;
  Y2dYPN(num)=(UNdYPN - Y2(num)*QN*Y2dYPN(num-1))*f;
  for K=(num-1):-1:1
    oldY2K = Y2(K);
    Y2(K)=oldY2K*Y2(K+1)+U(K);
    for I=1:num
      Y2dX(K,I)=Y2dX(K,I)*Y2(K+1)+oldY2K*Y2dX(K+1,I)+UdX(K,I);
      Y2dY(K,I)=Y2dY(K,I)*Y2(K+1)+oldY2K*Y2dY(K+1,I)+UdY(K,I);
    end
    Y2dYP1(K)=Y2dYP1(K)*Y2(K+1)+oldY2K*Y2dYP1(K+1)+UdYP1(K);
    Y2dYPN(K)=Y2dYPN(K)*Y2(K+1)+oldY2K*Y2dYPN(K+1);
  end
end

