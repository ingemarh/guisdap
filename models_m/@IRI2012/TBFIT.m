function [ THINT,TZERO ] = TBFIT( T1,T2,IBF )
%TBFIT TBFIT
%
%      SUBROUTINE TBFIT (T1,T2,IBF,THINT,TZERO)
%-------------------------------------------------------------------
%	COURTESY OF G.V. HAINES
%
%     T2    =  BEGINNING OF TIME INTERVAL.
%     T1    =  END OF TIME INTERVAL.
%     IBF   =  0   TO USE ORDINARY POLYNOMIALS AS TEMPORAL BASIS FUNCTIONS
%              1          LEGENDRE POLYNOMIALS
%              2          FOURIER SERIES
%              3          COSINE SERIES
%              4          SINE SERIES
%              5          COSINE + SINE SERIES
%     TZERO =  TIME-TRANSLATION PARAMETER:
%              FOR IBF.LE.1, CHOOSE TZERO = CENTER OF TIME INTERVAL
%              FOR IBF.GE.2, CHOOSE TZERO = BEGINNING OF TIME INTERVAL.
%     THINT =  TIME-SCALING PARAMETER. THINT = HALF OF TIME INTERVAL T2-T1
%              FOR IBF.LE.1; PI*HALF OF TIME INTERVAL FOR IBF.EQ.2;
%              AND PI*TIME INTERVAL FOR IBF.GE.3.
%     NOTE:    CHOOSING TZERO AND THINT IN THIS WAY SCALES TIME
%              TO (-1,1) FOR IBF.LE.1;  TO (0,2PI) FOR IBF.EQ.2;
%              AND TO (0,PI) FOR IBF.GE.3.
%-------------------------------------------------------------------

  IBFI  = IBF;
  if IBFI <= 1
    TZERO = (T2+T1)/2.;
  else
    TZERO =  T1;
  end
  THINT = T2 - T1;
  if IBFI <= 2
    THINT = THINT/2.;
  end
%      if (IBFI == 4)
%          DELT(0) = 0.0
%         else
%          DELT(0) = 1.0
%         end

end

