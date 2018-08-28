function [ D,T,ALT, context ] = GHP7( context, IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PRESS )
%GHP7 FIND ALTITUDE OF PRESSURE SURFACE
%      SUBROUTINE GHP7(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,
%        D,T,PRESS)
%-----------------------------------------------------------------------
%       FIND ALTITUDE OF PRESSURE SURFACE (PRESS) FROM GTD7
%     INPUT:
%        IYD - YEAR AND DAY AS YYDDD
%        SEC - UT(SEC)
%        GLAT - GEODETIC LATITUDE(DEG)
%        GLONG - GEODETIC LONGITUDE(DEG)
%        STL - LOCAL APPARENT SOLAR TIME(HRS)
%        F107A - 3 MONTH AVERAGE OF F10.7 FLUX
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
%             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 59 HRS PRIOR
%                    TO CURRENT TIME
%        PRESS - PRESSURE LEVEL(MB)
%     OUTPUT:
%        ALT - ALTITUDE(KM) 
%        D(1) - HE NUMBER DENSITY(CM-3)
%        D(2) - O NUMBER DENSITY(CM-3)
%        D(3) - N2 NUMBER DENSITY(CM-3)
%        D(4) - O2 NUMBER DENSITY(CM-3)
%        D(5) - AR NUMBER DENSITY(CM-3)
%        D(6) - TOTAL MASS DENSITY(GM/CM3)
%        D(7) - H NUMBER DENSITY(CM-3)
%        D(8) - N NUMBER DENSITY(CM-3)
%        D(9) - HOT O NUMBER DENSITY(CM-3)
%        T(1) - EXOSPHERIC TEMPERATURE
%        T(2) - TEMPERATURE AT ALT
%-----------------------------------------------------------------------

%      COMMON/PARMB/GSURF,RE
%      COMMON/METSEL/IMR
%      DIMENSION D(9),T(2),AP(7)
%      SAVE
  persistent TEST LTEST;
  
  if isempty(TEST)
    TEST = .00043;
    LTEST = 12;
  end
  D = zeros(CIRA.numDensities,1);
  PL=log10(PRESS);
%      Initial altitude estimate
  if PL>=-5.
    if PL > 2.5
      ZI=18.06*(3.00-PL);
    elseif PL > .75 %&& PL <= 2.5
      ZI=14.98*(3.08-PL);
    elseif PL > -1. %&& PL <= .75
      ZI=17.8*(2.72-PL);
    elseif PL > -2. %&& PL <= -1.
      ZI=14.28*(3.64-PL);
    elseif PL > -4. %&& PL <= -2.
      ZI=12.72*(4.32-PL);
    else %if PL <= -4.
      ZI=25.3*(.11-PL);
    end
    IDAY=mod(IYD,CIRA.YRDSHIFT);
    CL=GLAT/90.;
    CL2=CL*CL;
    daysPerYear = 365.0;
    if IDAY < (daysPerYear/2)
      CD=1.-IDAY/(daysPerYear/4);
    else %if IDAY >= (daysPerYear/2)
      CD=IDAY/(daysPerYear/4)-3.;
    end
    if PL > -.23
      CA=(2.79-PL)/(2.79+.23);
    elseif PL > -1.11 %&& PL <= -.23
      CA=1.0;
    elseif PL > -3. %&& PL <= -1.11
      CA=(-2.93-PL)/(-2.93+1.11);
    else%if PL <= -3
      CA=0;
    end
    Z=ZI-4.87*CL*CD*CA-1.64*CL2*CA+.31*CA*CL;
  else %if PL < -5.
    Z=22.*(PL+4.)^2+110;
  end
  %      ITERATION LOOP
  for L = 1:LTEST
    [D,T, context] = context.GTD7(IYD,SEC,Z,GLAT,GLONG,STL,F107A,F107,AP,...
      context.MT(CIRA.ALL_MASS),0.0);
    P = context.totalPressure( D, T );
    DIFF=PL-log10(P);
    if abs(DIFF) < TEST
      break;
    end
    XM=context.totalMolarMass( D ); % g/mol
    SH=context.SCALH(Z,context.RE(1,CIRA.Der0),context.GSURF(1,CIRA.Der0), ...
      XM,T(CIRA.TemperatureIndex(CIRA.Der0))); % km
    %         New altitude estimate using scale height
    if L < 6
      Z=Z-SH*DIFF*log(10.0);
    else
      Z=Z-SH*DIFF;
    end
  end
  if L >= LTEST && context.KONSOL > 0
    fprintf(context.KONSOL,'GHP7 NOT CONVERGING FOR PRESS, %12.2g mb,%12.2g\n', ...
      PRESS,DIFF);
  end
  ALT=Z;

end

