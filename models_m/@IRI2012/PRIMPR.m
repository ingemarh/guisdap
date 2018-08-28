function [ context ] = PRIMPR( context,IJ,Z,ZOX,ZN2,ZO2,HE,SZA,TN,F107,F107A,N4S )
%PRIMPR evaluates the ionization rates for photon impact
%
%.................... RSPRIM.FOR ..................................
%.... This routine evaluates the ionization rates for photon impact
%.... It is based on a FLIP model routine that was modified in August 
%.... 2009 for the chemical equilibrium model by P. richards. 
%      SUBROUTINE PRIMPR(IJ,Z,ZOX,ZN2,ZO2,HE,SZA,TN,F107,F107A,N4S)

%      IMPLICIT NONE
%      INTEGER IVERT,I,IJ,IK,IPROBS,IS,K,L,LMAX,NNI,K1
%      REAL EUVION,F107,F107A,F107SV,FNFAC,FREQLY,FREQSR,O2LYXS,
%     >  O2SRXS,TAUN,UVFAC,ZLAM,EUV,FLUXN,LAMAX,PEPION,PEXCIT,
%     >  SIGABS,SIGION,TPOT,ZFLUX
%      REAL Z,ZOX,ZN2,ZO2,HE,SZA,TN,CHI,ZZ,TNJ,TAU,FLUX,HEPLS,
%     >  FBSBN,DISN,TAUGAM,FLUXG,ALTG,XNSIGF,DSPECT,GL,N4S
%      REAL COLUM(3),OTHPR1,OTHPR2,OTHPR3(6)
%      REAL COLUMN(3),XN(3),PROB(3,6,37),XSNPLS(37),FNITE(37),CLNITE(3)
%pgr
%      REAL TPROB(3,6,37)
%pgr      

      %-- common to hold the context.EUV and photoelectron production rates 
%      COMMON/EUVPRD/EUVION(3,12),PEXCIT(3,12),PEPION(3,12),
%     >   OTHPR1(6),OTHPR2(6)
%      COMMON/SIGS/ZFLUX(37),SIGABS(3,37),ZLAM(37),SIGION(3,37),
%     > TPOT(3,10),NNI(3),LAMAX
%      COMMON/SOL/UVFAC(59),EUV
%      SAVE PROB,F107SV,TPROB    %.. Save values that are only calc once
  persistent FNITE FNFAC XSNPLS O2LYXS O2SRXS FREQSR;
  if isempty(FNITE)

    %.. Fluxes for nighttime ion production in the 37 wavelength bins of
    %.. Torr et al GRL 1979. The fluxes are set to reproduce the production
    %.. rates in Strobel et al. PSS, p1027, 1980. Note that most bins are 
    %.. set to zero and that the Strobel production rates are scaled by 
    %.. FNFAC to stabilize the O+ solution below 200 km. Note also that
    %.. the wavelengths in FNITE go from largest (#3=HI) to smallest.
    FNITE = [9E5,0.0,9E5,0.0,0.0,9E6,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, ...
             0.0,0.0,0.0,0.0,3E5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3E5,0.0, ...
             0.0,0.0,0.0,0.0,0.0,0.0,0.0];
    FNFAC = 1.0;
    O2LYXS = 1.0E-20;
    O2SRXS = 1.0E-21;
    FREQSR = 5.0E-6;
    %-- hv + N -> N+ + e. ion. freq. Richards et al. JGR 1994 page 8989
    XSNPLS = [0.0,0.0,0.0,0.0,0.0,0.0,.211,10.294,11.171,10.961,11.244,11.323 ...
      ,12.098,13.265,12.423,11.951,11.212,11.798,11.758,11.778,11.772,11.503 ...
      ,11.016,10.578,9.556,8.15,8.302,7.298,6.413,6.399,5.192,5.725 ...
      ,4.787,3.778,2.3,.878,.286];
  end
  %COLUM = zeros(3,1);

  %.. UVFAC(58) is left over from FLIP routines for compatibility
  context.UVFAC(58)=-1.0;
  if abs((F107-context.F107SV)/F107) > 0.005
    %.. update UV flux factors
    context.UVFAC = IRI2012.FACEUV(context.UVFAC,F107,F107A);
    context.UVFAC = IRI2012.FACSR(context.UVFAC,F107,F107A);

    %.. params to get solar flux data and cross sections
    [context,LMX] = context.PARAMS(0);
    context.LMAX = LMX;
    context.F107SV=F107;
  end

  %..  find probability for formation of each state  ........
  if context.IPROBS == 0
    PROB = IRI2012.PROBS(0,context.ZLAM,context.LMAX,context.NNI);
    for I=1:3
      for K=1:6
        for L=1:37
          context.TPROB(I,K,L)=PROB(I,K,L);
        end
      end
    end
    context.IPROBS=1;
  end

  %... initialization of production rates. 1.0E-15 stabilizes 
  %... e density evaluation at low altitudes in CMINOR
  for IS=1:3
    for IK=1:12
      context.EUVION(IS,IK)=1.0E-15;
    end
  end

  DISN=0.0;
  for I=1:6
    context.OTHPR2(I)=1.0E-15;
    context.OTHPR1(I)=1.0E-15;
  end
  %
  %........ Nighttime He+ production is calculated and stored. Attenuated to
  %........ avoid excess production at low altitudes
  context.OTHPR1(2)= 8E-11* exp(-1.0E-11*ZN2) *HE;
  %for I=1:3
  %  COLUM(I)=1.0E+25;
  %end
  TNJ=TN;
  XN(1)=ZOX;
  XN(2)=ZO2;
  XN(3)=ZN2;
  ZZ=Z*1.0E+5;
  CHI=SZA;
  %
  %*****  obtain reaction rates from subr rats to get their densities

  %....... determine if sun is below the horizon ...
  %---- Must now for calculation for night production - Feb 93
  %ALTG=(6371.0+Z)*sin(3.1416-CHI)-6371.0;
  %....      if CHI > 1.57.AND.ALTG < 85.) RETURN
  if Z > 1500
    return;
  end
  %
  %...... get column densities for scattered light at night  &&&&&&&&
  CLNITE = IRI2012.SCOLUM(IJ,0.0E0,ZZ,TNJ,XN);
  %
  %...... evaluate the neutral column density  &&&&&&&&
  COLUMN = IRI2012.SCOLUM(IJ,CHI,ZZ,TNJ,XN);
  %........ Store the column densities for the 2-Stream program
  %COLUM(1)=COLUMN(1);
  %COLUM(2)=COLUMN(2);
  %COLUM(3)=COLUMN(3);
  %
  %........ O2 dissociation by Schumann-Runge UV.
  %........ context.OTHPR1(3)= dissociation rate. context.OTHPR1(5)= Energy
  [context.OTHPR1(3),context.OTHPR1(5)] = context.SCHUMN(...
    IJ,Z,ZO2,COLUMN,context.OTHPR1(3),context.OTHPR1(5));
  %
  %---- Calculate hv + NO ion. freq. from Lyman-a (Brasseur & Solomon)
  %---- context.OTHPR2(2) is photodissociation of NO in the SR bands. 
  %---- A small night production from scattered light is included. FREQLY
  %---- varies with solar activity using Richards et al. 1994 page 8981
  %---- LY_a=2.5E11 (Lean), sigi(NO)=2.0E-18 (Brasseur & Solomon page 329)
   FREQLY=5.0E-7*(1+4.0E-3*(0.5*(F107+F107A)-80.0));
   context.OTHPR2(1)=FREQLY*(exp(-O2LYXS*COLUMN(2)) ...
                             +0.001*exp(-O2LYXS*CLNITE(2)));
   context.OTHPR2(2)=FREQSR*(exp(-O2SRXS*COLUMN(2)) ...
                             +0.001*exp(-O2SRXS*CLNITE(2)));

  %..  wavelength loop begins here  ----------
  %..  TAU, TAUN = optical depth for day, night 
  HEPLS=0.0;
  for L=1:context.LMAX
    TAU=0.;
    TAUN=0.0;
    for I=1:3
      TAUN=TAUN+context.SIGABS(I,L)*CLNITE(I);
      TAU=TAU+context.SIGABS(I,L)*COLUMN(I);
    end

    %.. evaluate nighttime flux and daytime flux
    FLUXN=FNFAC*(F107/75.0)*FNITE(L)*exp(-TAUN);
    FLUX=context.ZFLUX(L)*exp(-TAU) + FLUXN;
    %..WRITE(9,'(I6,1P,22E10.2)') L,COLUMN(1),COLUMN(3),TAU,exp(-TAU),
    %..>    FLUX, context.ZFLUX(L),FLUXN

    %.. he+ production. He+ X-S  = 0.25 N2  X-S. HEPRDN = nite He+
    if context.ZLAM(L) < 500.
      HEPLS=HEPLS+HE*0.25*context.SIGION(3,L)*FLUX;
    end


    context.OTHPR2(3)=context.OTHPR2(3)+XSNPLS(L)*1.0E-18*FLUX*N4S;

    if context.ZLAM(L) >= 600.0
      %...... calculation of total context.EUV absorption-ionization.....
      FBSBN=FLUX*(context.SIGABS(3,L)-context.SIGION(3,L))*XN(3);
      %.. Save energy absorbed in the photodissociative process
      context.OTHPR1(4)=context.OTHPR1(4)+1.24E+4*FBSBN/context.ZLAM(L);
      %.. production on atomic nitrogen by dissociation
      DISN=DISN+FBSBN;
      %..      if J == 1) WRITE(6,95) L,context.ZLAM(L),TAU,FLUX,FBSBN,DISN,HEPLS
      %95   FORMAT(I4,F9.1,1P,22E9.1)
      %.. take into account the large n2 absorption of lyman gamma(972.54)
      if floor(context.ZLAM(L)) == 975
        TAUGAM=370E-18*COLUMN(3);
        if TAUGAM > 70.0
          TAUGAM=70.0;
        end
        FLUXG=context.UVFAC(34) *0.82E+9 *exp(-TAUGAM);
        DISN=DISN+FLUXG*370E-18*XN(3);
      end
    end

    %***** species loop begins here *****
    for I=1:3
      XNSIGF=XN(I)*context.SIGION(I,L)*FLUX;
      K1=context.NNI(I);

      %.. dspect=# ions formed by w-l l by ionization of k state of species i
      for K=1:K1
        DSPECT=XNSIGF*context.TPROB(I,K,L);
        %.. store ion production rates .....
        context.EUVION(I,K)=context.EUVION(I,K)+DSPECT;

        %.. calculation of ion heating rate......
        context.EUVION(1,10)=context.EUVION(1,10)+DSPECT*context.TPOT(I,K);

        %p           write(6,'(3I4,1P,9E10.2)') I,K,L,PROB(I,K,L),TPROB(I,K,L)
        %p     >     ,XN(I),context.SIGION(I,L),FLUX,XNSIGF,context.EUVION(I,K),XSNPLS(1),FNITE(1)

      end
    end
  end

  %..---   wavelength loop ends here   -----------
  %
  %.........Store UV disoc of N2 2 atoms produced for every dissociation
  context.OTHPR1(1)=2.0*DISN;
  %........ Transfer He+ production to storage
  context.OTHPR1(2)=context.OTHPR1(2)+HEPLS;

  %p      WRITE(6,*) 'After 6: context.EUVION(1,1), context.EUVION(2,1), context.EUVION(3,1)'
  %p      write(6,'(1P,9E10.2)') context.EUVION(1,1), context.EUVION(2,1), context.EUVION(3,1)

end

