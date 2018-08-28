function [ BN,BE,BV,context ] = SCHNEVPD( context, RZ,FLAT,FLON,R,T,L )
%SCHNEVPD COMPUTES SPHERICAL CAP HARMONIC
%
%      SUBROUTINE SCHNEVPD (RZ,FLAT,FLON,R,T,L,BN,BE,BV)
%-------------------------------------------------------------------
%     WHEN L IS POSITIVE:
%     COMPUTES SPHERICAL CAP HARMONIC (GEOCENTRIC) FIELD COMPONENTS
%     HORIZONTAL NORTH BN,HORIZONTAL EAST BE,AND VERTICAL DOWNWARD BV.
%     WHEN L IS NEGATIVE:
%     COMPUTES GENERAL FUNCTION BV, ITS HORIZONTAL NORTH DERIVATIVE BN,
%     AND ITS HORIZONTAL EAST DERIVATIVE BE, ON SPHERICAL CAP SURFACE.
%     NOTE THAT THESE ARE METRICAL DERIVATIVES, AND BE IS THE
%     LONGITUDINAL DERIVATIVE DIVIDED BY SIN(COLATITUDE).
%
%     FLAT,FLON,R ARE GEOCENTRIC SPHERICAL CAP LATITUDE,LONGITUDE,RADIAL
%     DISTANCE; T IS TIME.
%
%     L =  0  ON FIRST CALL:  RETURNS SPHERICAL CAP POLE POSITION FLATO,FLONO
%             AND HALF-ANGLE THETA AS BN,BE, AND BV AFTER INITIALIZATION.
%             ON SUBSEQUENT CALLS:  ACTS AS L=1.
%          1  COMPUTES POTENTIAL FIELD COMPONENTS FROM INTERNAL COEFFICIENTS.
%          2  COMPUTES POTENTIAL FIELD COMPONENTS FROM EXTERNAL COEFFICIENTS.
%          3  COMPUTES FIELD FROM BOTH INTERNAL AND EXTERNAL COEFFICIENTS.
%         -1  COMPUTES GENERAL FUNCTION BV AND DERIVATIVES BN WITH RESPECT TO
%             LATITUDE AND BE WITH RESPECT TO LONGITUDE DIVIDED BY COS(LAT)
%             (R IS DUMMY VARIABLE IN THIS CASE).
%     NOTE:   SUBROUTINE IS INITIALIZED DURING FIRST CALL REGARDLESS OF L.
%
%     SUBPROGRAM USED:  LEGFUN
%
%	***** PARAMS & COEFFS TRANSFERRED FROM MAIN PROGRAM *****
%
%	ADAPTED FROM SUBROUTINE SCHNEV OF G.V. HAINES (COMPUTERS & GEOSCIENCES, 
%      14, 413-447, 1988)
%-------------------------------------------------------------------

%      PARAMETER   (IBO=0,JBO=1,KDIM=6,LDIM=4)                                     
%      DIMENSION   FN(0:KDIM,0:KDIM), CONST(0:KDIM,0:KDIM)
%      DIMENSION   CML(KDIM), SML(KDIM)
%      DIMENSION   DELT(0:LDIM)
%      DIMENSION   BINT(0:KDIM,0:KDIM,1-IBO-JBO:LDIM),
%     *            BEXT(0:KDIM,0:KDIM,1-IBO-JBO:LDIM)
%      COMMON      BINT,BEXT,RE,TZERO,IFIT,IB,KINT,LINT,KEXT,
%     *              LEXT,KMAX,FN
%C     ,CONST
%      CHARACTER*1 IE,RESP
%
  persistent IBO JBO KDIM LDIM CONST;
  if isempty(IBO)
    IBO=0;
    JBO=1;
    KDIM=6;
    LDIM=4;
    
    cnst = ...
      	 [1., ...
          1.,1., ...
          1.,1.73205,0.866025, ...
          1.,2.44949,1.93649,0.7905691, ...
          1.,3.16228,3.35410,2.09165,0.739510, ...
          1.,3.87298,5.12348,4.18330,2.21853,0.701561, ...
          1.,4.58258,7.24569,7.24569,4.96078,2.32681,0.671693];
    CONST = zeros(KDIM+1,KDIM+1);
    K = 1;
    for N=0:KDIM
      for M=0:N
        CONST(N+1,M+1) = cnst(K);
        %CONST(M+1,N+1) = cnst(K);
        K = K + 1;
      end
    end
  end
  BN = 0;
  BE = 0;
  BV = 0;
  DELT = zeros(LDIM+1,1);
  CML = zeros(KDIM,1);
  SML = zeros(KDIM,1);

%     IBF   =  0   TO USE ORDINARY POLYNOMIALS AS BASIS FUNCTIONS
%              1          LEGENDRE POLYNOMIALS
%              2          FOURIER SERIES
%              3          COSINE SERIES
%              4          SINE SERIES
%     NOTE:    TZERO AND THINT MAY DEPEND ON IBF.
  IBF   =  2;
  T1=1.;
  T2=12.;
  [THINT,context.TZERO] = IRI2012.TBFIT (T1,T2,IBF);

%      if (L == 0)
%        BN = FLATO;
%        BE = FLONO;
%        BV = THETA;
%        return;
%      end

  if L >= 0
    if context.IFIT < 0
      %stop
      if context.KONSOL > 0
        fprintf(context.KONSOL,'ERROR - bad L(%d) or IFIT(%d) in SCHNEVPD (%d < %d)\n',L,context.IFIT,context.IFIT,0);
      end
      return;
    end
    AOR = context.RE/R;
    AR = AOR^2;
    %if (L > 1)  GO TO 107
  else
    if context.IFIT >= 0
      %stop
      if context.KONSOL > 0
        fprintf(context.KONSOL,'ERROR - bad L(%d) or IFIT(%d) in SCHNEVPD (%d >= %d)\n',L,context.IFIT,context.IFIT,0);
      end
      return;
    end
    AR = -1.;
  end
  if L <= 1
    KT = context.LINT;
  else
    if context.KEXT > 0
      AOR3 = AOR*AR;
    end
    if L <= 2
      KT = context.LEXT;
    else
      KT = max (context.LINT,context.LEXT);
    end
  end
  DELT(0+1) = 1.;
  if KT > 0
    DEL = (T - context.TZERO)/THINT;
    for I=1:KT
      if I == 1
         if IBF <= 1
           DELT(I+1) = DEL;
         elseif IBF == 2
           ST = sin(DEL);
           DELT(I+1) = ST;
         elseif IBF == 3
           DELT(I+1) = cos(DEL);
         else
           DELT(I+1) = sin(DEL);
         end
      else
        if IBF == 0
          DELT(I+1) = DELT(I-1+1)*DEL;
        elseif IBF == 1
          RECIP = 1./I;
          DELT(I+1) = (2.-RECIP)*DELT(I-1+1)*DEL - (1.-RECIP)*DELT(I-2+1);
        elseif IBF == 2
           if floor(I/2)*2 == I
             if I == 2
                CT = cos(DEL);
                DELT(I+1) = CT;
             else
                DELT(I+1) = DELT(I-2+1)*CT - DELT(I-3+1)*ST;
             end
           else
             DELT(I+1) = DELT(I-2+1)*CT + DELT(I-1+1)*ST;
           end
        elseif IBF == 3
          DELT(I+1) = cos(I*DEL);
        elseif IBF == 4
          DELT(I+1) = sin(I*DEL);
        else
          %stop
          if context.KONSOL > 0
            fprintf(context.KONSOL,'ERROR - bad IBF(%d) in SCHNEVPD (%d > %d)\n',IBF,IBF,4);
          end
          return;
        end
      end
    end
    incept = 0;
    if (IBF == 2 || IBF == 3) && incept == 1
      %     change to intercept form of fourier series.
      for i=2:(4-IBF):context.LINT
        DELT(i+1) = 1. - DELT(i+1);
      end
    end
  end
  X = 0.;
  Y = 0.;
  Z = 0.;
  if L ~= 2
    if context.KINT >= 0
      GTI = 0.;
      for I=1-IBO-JBO:context.LINT
        GTI = GTI + context.BINT(0+1,0+1,I-(1-IBO-JBO)+1)*DELT(I+1);
      end
      Z = -AR*GTI;
      %N =  0;
    end
  end
  COLAT = 90. - FLAT;
  for N=1:context.KMAX
    if N == 1
      CL = cos(FLON*IRI2012.UMR);
      SL = sin(FLON*IRI2012.UMR);
      CML(1) = CL;
      SML(1) = SL;
    else
      SML(N) = SL*CML(N-1) + CL*SML(N-1);
      CML(N) = CL*CML(N-1) - SL*SML(N-1);
    end
    for M=0:N
      if context.IB ~= 2
        NMM = N - M;
        if floor(NMM/2)*2 == NMM
          continue;
        end
      end
      FFN = context.FN(N+1,M+1);
      [P,DP,PMS] = IRI2012.LEGFUN (M,FFN,CONST(N+1,M+1),COLAT,0);
      if L >= 0
        AR = AOR^(FFN+2.);
      else
        AR = 1.;
        FFN = -2.;
        DP = -DP;
        PMS = -PMS;
      end
      if M == 0
        BT1 = 0.;
        BT3 = 0.;
        BT  = 0.;
        if L ~= 2
          if N <= context.KINT
            GTI = 0.;
            for I=1-IBO-JBO:context.LINT
              GTI  = GTI  + context.BINT(N+1,M+1,I-(1-IBO-JBO)+1)*DELT(I+1);
            end
            BT1  = AR*GTI;
            BT3  = BT1;
          end
        end
        if L > 1
          if N <= context.KEXT
            GTE = 0.;
            for I=1-IBO-JBO:context.LEXT
              GTE = GTE + context.BEXT(N+1,M+1,I-(1-IBO-JBO)+1)*DELT(I+1);
            end
            BT  = AOR3/AR*GTE;
            BT1 = BT1 + BT;
          end
        end
        X = X + BT1*DP;
        Z = Z - (FFN*(BT3-BT)+BT3)*P;
      else
        BT1 = 0.;
        BT2 = 0.;
        BT3 = 0.;
        BT  = 0.;
        if L ~= 2
          if N <= context.KINT
            GTI = 0.;
            HTI = 0.;
            for I=1-IBO-JBO:context.LINT
              GTI = GTI + context.BINT(N+1,M+1,I-(1-IBO-JBO)+1)*DELT(I+1);
              HTI = HTI + context.BINT(M-1+1,N+1,I-(1-IBO-JBO)+1)*DELT(I+1);
            end
            BT1 = AR*(GTI*CML(M) + HTI*SML(M));
            BT2 = AR*(GTI*SML(M) - HTI*CML(M));
            BT3 = BT1;
          end
        end
        if L > 1
          if N <= context.KEXT
            GTE = 0.;
            HTE = 0.;
            for I=1-IBO-JBO:context.LEXT
              GTE = GTE + context.BEXT(N+1,M+1,I-(1-IBO-JBO)+1)*DELT(I+1);
              HTE = HTE + context.BEXT(M-1+1,N+1,I-(1-IBO-JBO)+1)*DELT(I+1);
            end
            RA = AOR3/AR;
            BT = RA*(GTE*CML(M) + HTE*SML(M));
            BT1 = BT1 + BT;
            BT2 = BT2 + RA*(GTE*SML(M) - HTE*CML(M));
          end
        end
        X = X + BT1*DP;
        Y = Y + BT2*PMS;
        Z = Z - (FFN*(BT3-BT)+BT3)*P;
      end
    end
  end
  BN = X;
  BE = Y;
  BV = Z;

end

