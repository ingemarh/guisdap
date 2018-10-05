function [ EDENS,IERROR,dEDENS ] = F00( HGT,GLAT1,IDAY,ZANG,F107T )
%F00 COMPUTES "FIRI" ELECTRON DENSITIES
% iridreg.for, version number can be found at the end of this comment.
%-----------------------------------------------------------------------
%
% This file contains the D-region models of Friedrich and Torkar (2001)
% (subroutine F00 and block DATA statement).
% The subroutine DRegion of Danilov et al. (1995) was moved to IRIFUN, ...
% because of consistent problems of some Fortran compilers wit the long
% BLOCK %DATA statement. 
%
% !!!USER NOTE!!! If your compiler has problems with this subroutine you 
% can compile IRI without this file. But you first have to comment out  
% the following two line in IRISUB: 
%            call F00(HEIGHT,LATI,DAYNR,XHI,F107D,EDENS,IERROR)
%            if(ierror == 0 || ierror == 2) outf(1,kk)=edens
%
%-----------------------------------------------------------------------
% Corrections];Version Numbers:
%-Version-mm];dd];yy-description (person reporting correction)
% 2001.01 05];07];01 initial version
% 2001.02 07];11];01 new version of F00 (as provided by K. Torkar)
% 2002.01 28];10];02 replace TAB];6 blanks, PARAMETER () (D. Simpson)
% 2007.00 05];18];07 Release of IRI-2007
% 2012.00 12];29];11 Release of IRI-2012; no change in iridreg.for
% 2012.00 01];18];12 Moved subroutine DRegion (Danilov model) to IRIFUN
%-----------------------------------------------------------------------
%
%
%     SUBROUTINE F00(HGT,GLAT1,IDAY,ZANG,F107T,EDENS,IERROR)
%---------------------------------------------------------------------
%     PURPOSE:
%     THIS SUBROUTINE COMPUTES "FIRI" ELECTRON DENSITIES
%
%     COMMON BLOCK REQUIRED:
%       REAL EDEN,TABHE,TABLA,TABMO,TABZA,TABFL
%       COMMON/FIRCOM/EDEN(81,5,12,12,3), ...
%      1              TABHE(81),TABLA(5),TABMO(12),TABZA(12),TABFL(3)
%
%       ARRAY EDEN contains LOG10(tabulated electron density, ...
%       ordered in (height,latitude,month,zenithangle,f107)
%       Quantity      Minimum  Maximum  Number of steps
%       Height        60       140      81
%       Latitude       0        60       5
%       Month          1        12      12
%       Zenith angle   0       180      12
%       F10.7         75       200       3
%
%     PARAMETERS:
%        HGT   height in km (input, REAL)
%        GLAT1 latitude in degrees, north is positive (input, REAL)
%        IDAY  day of the year (input, INTEGER)
%        ZANG  solar zenith angle in degrees (input, REAL)
%        F107T 10.7cm flux in Ja (input, REAL)
%        EDENS model electron density in m**-3 (output, REAL)
%        IERROR  Error code (INTEGER, output)
%
%       Error code
%         0         no error
%         1         model undefined for given combination of input
%                   parameters, output is set to zero
%         2         input parameters outside valid range, output is invalid
%         3         both error conditions detected, output is zero
%
%     USAGE
%        CALL F00(HGT,GLAT1,IDAY,ZANG,F107T,EDENS,IERROR)
%
%     SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
%        none
%
%     Reference: Friedrich, M., Torkar, K. FIRI: a semiempirical model of the lower
%                ionosphere. J. Geophys. Res. 106 (A10), 21409Ð21418, 2001.
%     WRITTEN BY K. TORKAR, IWF GRAZ
%     Klaus.Torkar@oeaw.ac.at
%
%     LAST MODIFICATION:  06.07.2001
%
%     VERSION: 1.1
%
%     ------------------------------------------------------------------

%      REAL HGT,GLAT1,ZANG,F107T,EDENS,F107L
%      INTEGER IDAY,IERROR
%
  persistent NHGT NLAT NMON NZEN NF10 EDEN TABHE TABLA TABZA TABFL TABM STEPJ ISTEPJ;
  if isempty(NHGT)
    NHGT=81;
    NLAT=5;
    NMON=12;
    NZEN=12;
    NF10=3;
    %     altitudes in km
    TABHE = [ ...
       60.,61.,62.,63.,64.,65.,66.,67.,68.,69., ...
       70.,71.,72.,73.,74.,75.,76.,77.,78.,79., ...
       80.,81.,82.,83.,84.,85.,86.,87.,88.,89., ...
       90.,91.,92.,93.,94.,95.,96.,97.,98.,99., ...
       100.,101.,102.,103.,104.,105.,106.,107.,108.,109., ...
       110.,111.,112.,113.,114.,115.,116.,117.,118.,119., ...
       120.,121.,122.,123.,124.,125.,126.,127.,128.,129., ...
       130.,131.,132.,133.,134.,135.,136.,137.,138.,139.,140.];
    %
    %     latitudes in degree
    TABLA = [ ...
       0.,15.,30.,45.,60.];
    %
    %     months
    %TABMO = [ ...
    %   1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.];
    %
    %     solar zenith angles in degree
    TABZA = [ ...
       0.,30.,45.,60.,75.,80.,85.,90.,95.,100.,130.,180.];
    %
    %     log10(F10.7) for 75,130,200 Jy
    TABFL = [ ...
       1.87506, 2.11394, 2.30103];
    %
    %     log10 electron densities, ordered as
    %     I,J,K,L,M = Height,Latitude,Month,Zenithangle,F10.7
    %     8 heights in each line
    %     12 zenith angles in each %DATA statement
    %     innermost loop: J (5 latitudes)
    %     next loop:      K (12 months)
    %     next loop:      M (3 F10.7-fluxes)
    %     outermost loop: I (11 groups of heights)
    EDEN = zeros(NHGT,NLAT,NMON,NZEN,NF10);
    fp = fopen('F00Data.dat','r');
    if fp < 0
      return;
    end
    sc = textscan(fp,'%f,');
    kk = 1;
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=1:8
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=9:16
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=17:24
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=25:32
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=33:40
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=41:48
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=49:56
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=57:64
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=65:72
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=73:80
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    for M=1:NF10
      for K=1:NMON
        for J=1:NLAT
          for L=1:NZEN
            for I=81:81
              EDEN(I,J,K,L,M) = sc{1}(kk);
              kk = kk + 1;
            end
          end
        end
      end
    end
    %
    %
    %      REAL EDEN,TABHE,TABLA,TABMO,TABZA,TABFL
    %      COMMON/FIRCOM/EDEN(81,5,12,12,3),
    %     1              TABHE(81),TABLA(5),TABMO(12),TABZA(12),TABFL(3)
    %      INTEGER MON,I,J,L,M,ISTEPJ,I1,I2,J1,J2,K1,K2,L1,L2,M1,M2
    %      INTEGER TABM(12)
    %      REAL EDENI(2,2,2,2),EDENIJ(2,2,2),EDENIJK(2,2),EDENIJKL(2)
    %      REAL STEPJ,DAY1,H1,DEG1,XHI1,FLX1,EL
    %
    TABM = [0,31,59,90,120,151,181,212,243,273,304,334];
    STEPJ=15.0;
    ISTEPJ=15;
  end
  EDENI = zeros(2,2,2,2);
  dEDENI = zeros(2,2,2,2);
  EDENIJ = zeros(2,2,2);
  dEDENIJ = zeros(2,2,2);
  EDENIJK = zeros(2,2);
  dEDENIJK = zeros(2,2);
  EDENIJKL = zeros(1,2);
  dEDENIJKL = zeros(1,2);
  %
  %     INDICES:
  %     I=HEIGHT, J=LATITUDE, K=MONTH, L=ZANG, M=F10.7
  %
  %     CHECK INPUT
  %
  IERROR=0;
  F107L=log10(min(1000.0,max(1.0,F107T)));
  if (HGT < TABHE(1) || ...
      HGT > TABHE(NHGT) || ...
      GLAT1 > TABLA(NLAT) || ...
      GLAT1 < -TABLA(NLAT) ||  ...
      IDAY < 1 || IDAY > 366 ||  ...
      ZANG < TABZA(1) || ...
      ZANG > TABZA(NZEN) ||  ...
      F107L < TABFL(1) || ...
      F107L > TABFL(NF10))
    IERROR=2;
  end
  %
  %     assume height table is in 1 km steps from 60 to 140 km
  I=min(NHGT-1,floor(HGT)-59);
  if(I < 1)
    I=1;
  end
  H1=HGT-TABHE(I);
  dH1=1.0;
  I1=I;
  I2=I+1;
  %
  %     assume latitude table is in 15 deg steps from 0 to 60 deg
  if length(GLAT1)>1
    x=x;
  end
  J=max(1,min(NLAT-1,floor(abs(GLAT1)/ISTEPJ)));
  DEG1=(abs(GLAT1)-TABLA(J))/STEPJ;
  J1=J;
  J2=J+1;
  %
  %     assume month table is given for each month
  MON=12;
  while (TABM(MON) > IDAY)
    MON=MON-1;
  end
  DAY1=double(IDAY-TABM(MON)-15)/30.0;
  if (DAY1 < 0.0)
    MON=MON-1;
  end
  if(MON >= 1 && MON <= 11)
    K1=MON;
    K2=MON+1;
  else
    K1=12;
    K2=1;
  end
  %
  %     assume zenith angle table has 12 entries between 0 and 180 deg
  for L=2:NZEN-1
    if(ZANG < TABZA(L))
      break;
    end
  end
  if L >= NZEN
    L=NZEN;
  else
    L=L-1;
  end
  L1=L;
  L2=L+1;
  XHI1=(ZANG-TABZA(L1))/(TABZA(L2)-TABZA(L1));
  %
  %     assume solar activity table has 3 entries
  F107L=min(TABFL(3),max(TABFL(1),F107L));
  if(F107L < TABFL(NF10-1))
    M1=1;
    M2=2;
  else
    M1=2;
    M2=3;
  end
  FLX1=(F107L-TABFL(M1))/(TABFL(M2)-TABFL(M1));
  %
  %     ADJUST SOUTHERN LATITUDES TO NORTH AND MONTH+6
  %
  if(GLAT1 < 0.0)
    K1=K1+6;
    if(K1 > 12)
      K1=K1-12;
    end
    K2=K2+6;
    if(K2 > 12)
      K2=K2-12;
    end
  end
  %
  %     EDEN(hgt,lat,mon,zang,f107)
  %          I   J   K   L    M
  %
  for M=M1:M2
    %
    MH=M+1-M1;
    %       INTERPOLATE IN HEIGHT I
    for L=L1:L2
      if(EDEN(I1,J1,K1,L,M) == 0.0 || ...
         EDEN(I2,J1,K1,L,M) == 0.0 || ...
         EDEN(I1,J2,K1,L,M) == 0.0 || ...
         EDEN(I2,J2,K1,L,M) == 0.0 || ...
         EDEN(I1,J1,K2,L,M) == 0.0 || ...
         EDEN(I2,J1,K2,L,M) == 0.0 || ...
         EDEN(I1,J2,K2,L,M) == 0.0 || ...
         EDEN(I2,J2,K2,L,M) == 0.0)
        EDENS=0.0;
        dEDENS=0.0;
        IERROR=IERROR+1;
        return;
      end
      if(HGT < TABHE(1))
        EDENI(1,1,L+1-L1,MH)=EDEN(I1,J1,K1,L,M);
        EDENI(2,1,L+1-L1,MH)=EDEN(I1,J2,K1,L,M);
        EDENI(1,2,L+1-L1,MH)=EDEN(I1,J1,K2,L,M);
        EDENI(2,2,L+1-L1,MH)=EDEN(I1,J2,K2,L,M);
        dEDENI(1,1,L+1-L1,MH)=0.0;
        dEDENI(2,1,L+1-L1,MH)=0.0;
        dEDENI(1,2,L+1-L1,MH)=0.0;
        dEDENI(2,2,L+1-L1,MH)=0.0;
      elseif(HGT > TABHE(NHGT))
        EDENI(1,1,L+1-L1,MH)=EDEN(I2,J1,K1,L,M);
        EDENI(2,1,L+1-L1,MH)=EDEN(I2,J2,K1,L,M);
        EDENI(1,2,L+1-L1,MH)=EDEN(I2,J1,K2,L,M);
        EDENI(2,2,L+1-L1,MH)=EDEN(I2,J2,K2,L,M);
        dEDENI(1,1,L+1-L1,MH)=0.0;
        dEDENI(2,1,L+1-L1,MH)=0.0;
        dEDENI(1,2,L+1-L1,MH)=0.0;
        dEDENI(2,2,L+1-L1,MH)=0.0;
      else
        EDENI(1,1,L+1-L1,MH)=EDEN(I1,J1,K1,L,M)+ ...
          H1*(EDEN(I2,J1,K1,L,M)-EDEN(I1,J1,K1,L,M));
        EDENI(2,1,L+1-L1,MH)=EDEN(I1,J2,K1,L,M)+ ...
          H1*(EDEN(I2,J2,K1,L,M)-EDEN(I1,J2,K1,L,M));
        EDENI(1,2,L+1-L1,MH)=EDEN(I1,J1,K2,L,M)+ ...
          H1*(EDEN(I2,J1,K2,L,M)-EDEN(I1,J1,K2,L,M));
        EDENI(2,2,L+1-L1,MH)=EDEN(I1,J2,K2,L,M)+ ...
          H1*(EDEN(I2,J2,K2,L,M)-EDEN(I1,J2,K2,L,M));
        dEDENI(1,1,L+1-L1,MH)=dH1*(EDEN(I2,J1,K1,L,M)-EDEN(I1,J1,K1,L,M));
        dEDENI(2,1,L+1-L1,MH)=dH1*(EDEN(I2,J2,K1,L,M)-EDEN(I1,J2,K1,L,M));
        dEDENI(1,2,L+1-L1,MH)=dH1*(EDEN(I2,J1,K2,L,M)-EDEN(I1,J1,K2,L,M));
        dEDENI(2,2,L+1-L1,MH)=dH1*(EDEN(I2,J2,K2,L,M)-EDEN(I1,J2,K2,L,M));
      end
    end
    %
    %       INTERPOLATE IN LATITUDE J
    for L=1:2
      if(abs(GLAT1) > TABLA(NLAT))
        EDENIJ(1,L,MH)=EDENI(2,1,L,MH);
        EDENIJ(2,L,MH)=EDENI(2,2,L,MH);
        dEDENIJ(1,L,MH)=dEDENI(2,1,L,MH);
        dEDENIJ(2,L,MH)=dEDENI(2,2,L,MH);
      else
        EDENIJ(1,L,MH)=EDENI(1,1,L,MH)+ ...
          DEG1*(EDENI(2,1,L,MH)-EDENI(1,1,L,MH));
        EDENIJ(2,L,MH)=EDENI(1,2,L,MH)+ ...
          DEG1*(EDENI(2,2,L,MH)-EDENI(1,2,L,MH));
        dEDENIJ(1,L,MH)=dEDENI(1,1,L,MH)+ ...
          DEG1*(dEDENI(2,1,L,MH)-dEDENI(1,1,L,MH));
        dEDENIJ(2,L,MH)=dEDENI(1,2,L,MH)+ ...
          DEG1*(dEDENI(2,2,L,MH)-dEDENI(1,2,L,MH));
      end
    end
    %
    %       INTERPOLATE IN MONTH K
    EDENIJK(1,MH)=EDENIJ(1,1,MH)+ ...
       DAY1*(EDENIJ(2,1,MH)-EDENIJ(1,1,MH));
    EDENIJK(2,MH)=EDENIJ(1,2,MH)+ ...
       DAY1*(EDENIJ(2,2,MH)-EDENIJ(1,2,MH));
    dEDENIJK(1,MH)=dEDENIJ(1,1,MH)+ ...
       DAY1*(dEDENIJ(2,1,MH)-dEDENIJ(1,1,MH));
    dEDENIJK(2,MH)=dEDENIJ(1,2,MH)+ ...
       DAY1*(dEDENIJ(2,2,MH)-dEDENIJ(1,2,MH));
    %
    %       INTERPOLATE IN ZENITH ANGLE L
    EDENIJKL(MH)=EDENIJK(1,MH)+XHI1*(EDENIJK(2,MH)-EDENIJK(1,MH));
    dEDENIJKL(MH)=dEDENIJK(1,MH)+XHI1*(dEDENIJK(2,MH)-dEDENIJK(1,MH));
  end

  EL=EDENIJKL(1)+FLX1*(EDENIJKL(2)-EDENIJKL(1));
  dEL=dEDENIJKL(1)+FLX1*(dEDENIJKL(2)-dEDENIJKL(1));

  EDENS=10.^EL;
  dEDENS=EDENS*log(10.0)*dEL;

end

