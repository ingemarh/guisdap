function [ D,T,context ] = GTD7D( context, IYD,SEC,ALT,GLAT,GLONG,STL, ...
  F107A,F107,AP,MASS,D1 )
%GTD7D This subroutine provides Effective Total Mass Density for output D(6)
%      SUBROUTINE GTD7D(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,
%       D,T)
%-----------------------------------------------------------------------
%
%     NRLMSISE-00
%     -----------
%        This subroutine provides Effective Total Mass Density for
%        output D(6) which includes contributions from "anomalous
%        oxygen" which can affect satellite drag above 500 km.  This
%        subroutine is part of the distribution package for the 
%        Neutral Atmosphere Empirical Model from the surface to lower
%        exosphere.  See subroutine GTD7 for more extensive comments.
%
%     INPUT VARIABLES:
%        IYD - YEAR AND DAY AS YYDDD (day of year from 1 to 365 (or 366))
%              (Year ignored in current model)
%        SEC - UT(SEC)
%        ALT - ALTITUDE(KM)
%        GLAT - GEODETIC LATITUDE(DEG)
%        GLONG - GEODETIC LONGITUDE(DEG)
%        STL - LOCAL APPARENT SOLAR TIME(HRS; see Note below)
%        F107A - 81 day AVERAGE OF F10.7 FLUX (centered on day DDD)
%        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
%        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
%           - ARRAY CONTAINING:
%             (1) DAILY AP
%             (2) 3 HR AP INDEX FOR CURRENT TIME
%             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
%             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
%             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
%             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
%                    TO CURRENT TIME
%             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR
%                    TO CURRENT TIME
%        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
%                 CALCULATED.  MASS 0 IS TEMPERATURE.  MASS 48 FOR ALL.
%                 MASS 17 IS Anomalous O ONLY.)
%
%     NOTES ON INPUT VARIABLES: 
%        UT, Local Time, and Longitude are used independently in the
%        model and are not of equal importance for every situation.  
%        For the most physically realistic calculation these three
%        variables should be consistent (STL=SEC/3600+GLONG/15).
%        The Equation of Time departures from the above formula
%        for apparent local time can be included if available but
%        are of minor importance.
%
%        F107 and F107A values used to generate the model correspond
%        to the 10.7 cm radio flux at the actual distance of the Earth
%        from the Sun rather than the radio flux at 1 AU.
%
%     OUTPUT VARIABLES:
%        D(1) - HE NUMBER DENSITY(CM-3)
%        D(2) - O NUMBER DENSITY(CM-3)
%        D(3) - N2 NUMBER DENSITY(CM-3)
%        D(4) - O2 NUMBER DENSITY(CM-3)
%        D(5) - AR NUMBER DENSITY(CM-3)                       
%        D(6) - TOTAL MASS DENSITY(GM/CM3) [includes anomalous oxygen]
%        D(7) - H NUMBER DENSITY(CM-3)
%        D(8) - N NUMBER DENSITY(CM-3)
%        D(9) - Anomalous oxygen NUMBER DENSITY(CM-3)
%        T(1) - EXOSPHERIC TEMPERATURE (K)
%        T(2) - TEMPERATURE AT ALT (K)
%        T(3) - TEMPERATURE GRADIENT AT ALT (K/km)
%-----------------------------------------------------------------------

  [D,T,context] = context.GTD7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D1);
  %       TOTAL MASS DENSITY
  %
  if MASS == context.MT(CIRA.ALL_MASS)
    for d=CIRA.Der0:CIRA.DerLast
      if d == CIRA.DerSTL
        continue;
      end
      td = 0.0;
      for m = CIRA.HE_MASS:CIRA.N_MASS
        if m ~= CIRA.HOT_O_MASS
          td = td + context.MT(m)*D(CIRA.DensityIndex(d,m));
        end
      end
      td = td + context.MT(CIRA.O_MASS)*D(CIRA.DensityIndex(d,CIRA.ANOM_O_MASS));
      D(CIRA.DensityIndex(d,CIRA.ALL_MASS)) = CIRA.INVAVOG*td;
      if context.IMR == 1
        D(CIRA.DensityIndex(d,CIRA.ALL_MASS))=D(CIRA.DensityIndex(d,CIRA.ALL_MASS))*CIRA.KGPERGM;
      end
    end
  end

end

