function [ INTERP ] = INTERP( N,L,V,K,X,XOUT )
%INTERP INTERPOLATION

%       REAL FUNCTION INTERP(N,L,V,X,XOUT)
%-------------------------------------------------------------------------------

%        INTEGER N,L,S,S0,I
%        REAL V(N),X(N),XOUT,Y2(N),YOUT,X0(4),V0(4)
%        REAL XA,XB,XC,VA,VB,VC
  X0 = zeros(4,1);
  V0 = zeros(4,1);
  S = IRI2012.LOCATE(X,N,XOUT);
  if L == 0
    %       Spline interpolation (L=0)
    if S < 2
      S=2;
    elseif S > (N-2)
      S=N-2;
    end
    S0=S-1;
    for I=1:4
      X0(I)=X(S0+I-1);
      V0(I)=V(S0+I-1,K);
    end
    Y2 = IRI2012.SPLINE(X0,V0,4,1e30,1e30);
    YOUT = IRI2012.SPLINT(X0,V0,Y2,4,XOUT);
  elseif L == 1
    %       Linear interpolation (L=1)
    if (S >= 1) && (S < N)
      YOUT=(V(S+1,K)-V(S,K))/(X(S+1)-X(S))*(XOUT-X(S))+V(S,K);
    elseif S == 0
      YOUT=(V(2,K)-V(1,K))/(X(2)-X(1))*(XOUT-X(1))+V(1,K);
    else %if S == N
      YOUT=(V(N,K)-V(N-1,K))/(X(N)-X(N-1))*(XOUT-X(N))+V(N,K);
    end
  else %if L == 2
    %       Quadratic interpolation (L=2)
    if S < 2
      S=2;
    elseif S > (N-1)
      S=N-1;
    end
    XA=X(S-1);
    XB=X(S);
    XC=X(S+1);
    VA=V(S-1,K);
    VB=V(S,K);
    VC=V(S+1,K);
    YOUT=VA*(XOUT-XB)*(XOUT-XC)/((XA-XB)*(XA-XC))+ ...
         VB*(XOUT-XA)*(XOUT-XC)/((XB-XA)*(XB-XC))+ ...
         VC*(XOUT-XA)*(XOUT-XB)/((XC-XA)*(XC-XB));
  end
  INTERP=YOUT;

end

