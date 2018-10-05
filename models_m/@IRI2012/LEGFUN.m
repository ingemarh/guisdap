function [ P,DP,PMS ] = LEGFUN( M,FN,CONST,COLAT,IPRT )
%LEGFUN SERIES FORM FOR ASSOCIATED LEGENDRE FUNCTION
%
%      SUBROUTINE LEGFUN (M,FN,CONST,COLAT,P,DP,PMS,IPRT)
%-------------------------------------------------------------------
%     SERIES FORM FOR ASSOCIATED LEGENDRE FUNCTION P, ITS DERIVATIVE DP,
%     AND THE FUNCTION PMS=P*M/SIN(COLAT), IN POWERS OF (1-COS(COLAT))/2.
%     INTEGRAL ORDER M, REAL DEGREE FN, NORMALIZING CONSTANT CONST.
%     COLATITUDE COLAT IN DEGREES.
%     IPRT = 0     NO PRINT-OUT
%            1     PRINT PARAMETERS AND P SERIES
%            2     PRINT PARAMETERS AND DP SERIES
%            3     PRINT PARAMETERS AND BOTH P AND DP SERIES
%           -1     PRINT PARAMETERS ONLY
%     INPUT M,FN,CONST,COLAT,IPRT.   OUTPUT P,DP,PMS
%	ADAPTED FROM G.V. HAINES (COMPUTERS & GEOSCIENCES, 14, 413-447, 1988).
%-------------------------------------------------------------------

%      REAL*8  FNN,AL,A,B,PNM,DPNM
%      DIMENSION  AM(60), BM(60)
  persistent JMAX;
  if isempty(JMAX)
    JMAX = 60;
  end
  AM = zeros(JMAX,1);
  BM = zeros(JMAX,1);
  good = false;

  FNN = FN*(FN+1.);
  if (COLAT < 60.)
    X = sin(IRI2012.UMR*COLAT/2.)^2;
    C = 1. - 2.*X;
  else
    C = cos(COLAT*IRI2012.UMR);
    X = (1. - C)/2.;
  end
  S = sin(COLAT*IRI2012.UMR);
  if (M <= 1)
    if (M < 0)
      %STOP
      fprintf(1,'ERROR - bad M(%d) in LEGFUN (%d < %d)\n',M,M,0);
      return;
    end
    AL = CONST;
  else
    AL = CONST*S^(M-1);
  end
  PNM = AL;
  DPNM = 0.;
  for J=1:JMAX-1
    JPM = J + M;
    B = AL*((JPM-1)-FNN/JPM);
    DPNM = DPNM + B;
    A = (B*X)/J;
    PNM = PNM + A;
    AL = A;
    %     STORE P OR DP SERIES FOR PRINTOUT.
    if (IPRT > 0)
      if (IPRT == 2)
        BM(J) = B;
      else
        AM(J) = A;
        if (IPRT ~= 1)
          BM(J) = B;
        end
      end
    end
    %     CHECK FOR TERMINATION OF SERIES.
    ABSA = abs(A);
    ABSB = abs(B);
    if (ABSB < 1.E-7 && ABSA < 1.E-7)
      good = true;
      break;
    elseif (ABSB >= 1.E+13 || ABSA >= 1.E+13)
      good = false;
      break;
    end
  end
  %     CHANGE CHECK LIMITS ACCORDING TO ACCURACY DESIRED AND ACCORDING
  %     TO WORD SIZE OF COMPUTER.
  %     FOR 32-BIT WORD, DOUBLE PRECISION, E-8 AND E+8 GIVE 7 DIGITS ACCURACY.
  %     FOR 60-BIT WORD, DOUBLE PRECISION, E-15 AND E+15 GIVE 14 DIGITS ACCURACY.
  %     FOR 60-BIT WORD, SINGLE PRECISION, E-8 AND E+7 GIVE 7 DIGITS ACCURACY.
  %     (DOUBLE OR SINGLE PRECISION REFER TO FNN,AL,A,B,PNM,DPNM)
  if ~good
    %     CONVERGENCE SLOW OR JMAX TOO SMALL
    %     NUMERICAL ERROR UNACCEPTABLY LARGE DUE TO ADDING OF
    %     LARGE AND SMALL NUMBERS.
    fprintf(1,' ** ERROR ** %5d,%10.5f,%15.7f,%5d,%15.7g,%15.7g\n', M,FN,CONST,J,A,B);
    %STOP
    return;
  end
  %     SERIES TRUNCATED SUCCESSFULLY.
  PS = PNM;
  DPS = DPNM;
  if M == 0
    PMS = 0.;
    P = PS;
    DP = DPS*S/2.;
  else
    PMS = PS*M;
    P = PS*S;
    DP = DPS*S*S/2. + C*PMS;
  end
  %     PRINT TERMS OF SERIES
  if (IPRT == 0)
    return;
  end
  fprintf(1,' ,%5d,%10.5g,%20.12g,%10.2g,%25.14g,%25.14g,%25.14g,%5d\n',  M,FN,CONST,COLAT,P,DP,PMS,J);
  if (IPRT < 0)
    return;
  end
  if (IPRT ~= 2)
    for I=1:J
      fprintf(1,' ,%8.1g', AM(I));
    end
    if (IPRT == 1)
      return;
    end
  end
  %  135 PRINT *, (BM(I),I=1,J)

end

