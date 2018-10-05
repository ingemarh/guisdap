function [ OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT ] = CHEMION( context, ...
  JPRINT,ALT,F107,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,USER_NO,N4S,NE,USER_NPLUS,SZAD  )
%CHEMION IDC model
% flipiri.for 
%
% 2012.00 10/05/11 IRI-2012: bottomside B0 B1 model (SHAMDB0D, SHAB1D),
% 2012.00 10/05/11    bottomside Ni model (iriflip.for), auroral foE
% 2012.00 10/05/11    storm model (storme_ap), Te with PF10.7 (elteik),
% 2012.00 10/05/11    oval kp model (auroral_boundary), IGRF-11(igrf.for), 
% 2012.00 10/05/11    NRLMSIS00 (cira.for), CGM coordinates, F10.7 daily
% 2012.00 10/05/11    81-day 365-day indices (apf107.dat), ap->kp (ckp),
% 2012.00 10/05/11    array size change jf(50) outf(20,1000), oarr(100).
% 2012.01 12/12/11 Deleted ALT_RATES (not used)
% 2012.01 01/04/12 Deleted FINDAP,READAP,CONV_DATE,GET_DATA,RATCHK (not used)
% 2012.01 01/04/12 Deleted BRACE,ACTUAL_DAY,EPHEM SOLDEC,TFILE,RUN_ERROR (not used)
% 2012.01 01/04/12 COP2D: 99 FOMRAT ',' missing; commented out all WRITEs
%
%****************************************************************************************
% subroutines for IDC model
%
% includes: main subroutine CHEMION and the following subroutines and functions
%           KEMPPRN.FOR: CN2D, CNO, CN4S, CN2PLS, CNOP, CO2P, COP4S, COP2D, COP2P,
%                        CNPLS, CN2A, CN2P, CNOPV
%           RATES.FOR:   RATS 
%           PESIMP.FOR:  SECIPRD, FLXCAL, FACFLX, SIGEXS, TXSION, OXRAT, T_XS_N2, 
%                        T_XS_OX, OXSIGS
%           RSPRIM.FOR:  PRIMPR, SCOLUM, PARAMS, PROBS, PROBN2, YLDISS, PROBO2, 
%                        SCHUMN, FACEUV, FACSR,  
%  
% turn on printout of intermediate quantities with JPRINT=1 also in PARAMS, PROBS, 
% PROBN2, YLDISS, and PROBO2 with ISW=1.
% 
% Richards, P. G., D. Bilitza, and D. Voglozin (2010), Ion density calculator (IDC): 
% 	A new efficient model of ionospheric ion densities, Radio Sci., 45, RS5007, 
%   doi:10.1029/2009RS004332.
%
%****************************************************************************************
%
%
%      SUBROUTINE CHEMION(JPRINT,  %.. Input: Turn file output on or off
%     >                      ALT,  %.. Input: Altitude(km)
%     >               F107,F107A,  %.. Input: Solar activity indices
%     >                 TE,TI,TN,  %.. Input: Electron, ion and neutral temperatures
%     >          OXN,O2N,N2N,HEN,  %.. Input: O, O2, N2, and He densities (cm-3)
%     >                  USER_NO,  %.. Input: User specified NO density (cm-3)
%     >                      N4S,  %.. Input: N4S should be 0.5*MSIS N density (cm-3)
%     >                       NE,  %.. Input: electron density (cm-3)
%     >               USER_NPLUS,  %.. Input: User specified N+ density (cm-3)
%     >                     SZAD,  %.. Input: solar zenith angle(D)
%     >     OXPLUS,O2PLUS,NOPLUS,  %.. OUTPUT: O+, O2+, NO+ densities (cm-3)
%     >             N2PLUS,NPLUS,  %.. OUTPUT: N2+ and N+ densities (cm-3)
%     >                  NNO,N2D,  %.. OUTPUT: NO and N(2D) density (cm-3)
%     >                    INEWT)  %.. OUTPUT: Newton procedure fails if INEWT=0
%-------------------------------------------------------------------------------------
%... This routine was written by Phil Richards April 2010. 
%... It takes the specified input electron density and returns O+, O2+, NO+,
%... N2+, N+, NO, and N(2D) densities.These densities generally agree well
%... with the FLIP model densities.
%... All the densities except O+ are calculated from chemical equilibrium.
%... O+ is calculated using a Newton iterative procedure so that the total 
%... ion density matches the input electron density.
%... N+ and NO densities can either be user specified or calculated by the model.
%... N+ generally agrees well with AE-C data and the FLIP model during the day, 
%... but is inaccurate at night due to lack of diffusion.
%... NO will be very good except below about 130 km where it will be 
%... underestimated due to neglect of diffusion. There is an artificial 
%... floor on the NO density to prevent it from getting too low below 130 km.
%... If the Newton procedure fails to converge, all ions including O+ are 
%... calculated from chemical equilibrium and then normalized to reproduce 
%... the input electron density. This generally works well.
%... The Newton procedure usually works if the total calculated molecular ion 
%... densities do not approach the input electron density. Difficulties are most   
%... likely to happen below ~150 km and especially at night. A Newton solution is 
%... usually found when the O+ density is not too much smaller than the modeled 
%... molecular ion density.
%... The EUVAC model is used for solar EUV irradiances
%-------------------------------------------------------------------------------------

%      IMPLICIT NONE
%      INTEGER JPRINT        %.. Turns on printing of production and loss
%      INTEGER INEWT         %.. Signifies when the Newton procedure fails
%      INTEGER I,J,K,ITERS   %.. loop control variables
%      INTEGER IRATS         %.. Switch for different rates
%      INTEGER ITS,JITER     %.. Variables for Newton procedure
%      REAL TE,TN,TI         %.. Electron and ion temperatures
%      %.. Geophysical parameters
%      REAL F107,F107A,ALT,SZAD
%      %.. Measured H+, He+, O+, N2+, NO+, O2+, N+, RPA ion density
%      REAL HEPLUS,OXPLUS,N2PLUS,NOPLUS,O2PLUS,NPLUS,USER_NPLUS
%      %.. O2,O,N2,NO,He,N4S, user specified NO
%      REAL O2N,OXN,N2N,NNO,HEN,N4S,USER_NO
%      %.. Ne, N(2P),N(2D),O+(2P),O+(2D) densities
%      REAL NE,N2P,N2D,OP2D,OP2P
%      %.. Total (photon & photoel) production rates O+(4S),O+(2P),O+(2D),O2+
%      REAL TPROD1,PDISOP,TPROD2,TPROD3,TPROD5
%      %.. Total Production rates from all sources for NO+, O2+, 
%      REAL TPNOP,O2PPROD
%      %.. Production rates hv(e*)+N2->N+, hv+N->N+, Lyman-a -> NO+ 
%      REAL DISNP,PHOTN,PLYNOP
%      REAL PSEC                     %.. generic PE production
%      REAL RTS(99)                  %.. Reaction rates array
%      REAL SECPN2PLUS,EUVN2PLUS     %.. N2+ total production
%      REAL H,DEX,FEX(2)             %.. used in Newton solver
%      REAL SUMIONS                  %.. Sum of the major ions
%      REAL PNO,LNO,PDNOSR           %.. Production and loss of NO
%      REAL N2A                      %.. N2(A) density    
%      REAL VCON                     %.. FLIP N2(v) factor. Not used here
%      REAL DISN2D,UVDISN,PN2D,LN2D  %.. Production and loss of N(2D)
%      REAL ALTCHEM                  %.. altitude for chemistry calculation
%      REAL N2APRD                   %.. PE production rate of N2(A)
%      REAL PN4S,LN4S,DISN4S
%      REAL OXPLUSAVE
%      %.. various ionization and excitation rates by EUV and PE
%      REAL EUVION,PEXCIT,PEPION,OTHPR1,OTHPR2
%      COMMON/EUVPRD/EUVION(3,12),PEXCIT(3,12),PEPION(3,12),OTHPR1(6)
%     >   ,OTHPR2(6)
  persistent VCON PLYNOP;
  if isempty(VCON)
    %.. initialize parameters
    VCON = 1.0;
    PLYNOP = 0.0;
  end
  %PNO = 0.0;
  %LNO = 0.0;
  %PDNOSR = 0.0;
  %N2A = 0.0;
  K = 0;
  %DISN2D = 0.0;
  %UVDISN = 0.0;
  %HEPLUS = 0.0;
  FEX = zeros(2,1);
  NNO = 0.0;
  ALTCHEM=150;  %.. Initial altitude for O+ for imposing chemistry
  JITER=0;      %.. Counts the number of Newton iterations
  %N2P=0.0;      %.. N(2P) density

  RTS = IRI2012.RATS(0,TE,TI,TN);  %.. Get the reaction rates

  %.. PRIMPR calculates solar EUV production rates. 
  %      print*,'ALT,OXN,N2N,O2N,HEN,SZAD*0.01745,TN,F107,F107A,N4S'
  %      print*,ALT,OXN,N2N,O2N,HEN,SZAD*0.01745,TN,F107,F107A,N4S
  %UVDISN=context.OTHPR1(1);
  context = context.PRIMPR(1,ALT,OXN,N2N,O2N,HEN,SZAD*0.01745,TN,F107,F107A,N4S);
  UVDISN=context.OTHPR1(1);
  %.. Calculate secondary Production from photoelectrons
  [N2APRD,context] = context.SECIPRD(ALT,SZAD,F107,F107A,TE,TN,OXN,O2N,N2N,NE);

  DISNP = context.EUVION(3,4) ...
         +context.EUVION(3,5) ...
         +context.EUVION(3,6) ...
         +0.1*(context.PEPION(3,1) ...
              +context.PEPION(3,2) ...
              +context.PEPION(3,3)) ...  %.. Rydberg diss       
              +context.PEPION(3,4) ...
              +context.PEPION(3,5) ...
              +context.PEPION(3,6);
  DISN2D=2.0*context.PEPION(3,1)+context.OTHPR2(3);
  %DISN4S=2.0*context.PEPION(3,1)+context.OTHPR2(3);
   
  %**********  Come back here if Newton fails
  OXPLUS=0.0;
  SUMIONS=1.0;
  while OXPLUS/SUMIONS < 0.1
    %.. initialize variables to avoid using left over values
    HEPLUS=0.0;
    OXPLUS=0.0;
    N2PLUS=0.0;
    NOPLUS=0.0;
    O2PLUS=0.0;
    NPLUS=0.0;
    N2P=0.0;
    N2D=0.0;
    OP2D=0.0;
    OP2P=0.0;
    N2A=0.0;

    %.. Iterate through chemistry twice in case O+ is chemical equilibrium
    for ITERS=1:3
      %.. K counts number of iterations for printing headers in routines
      K=K+1;
      %.. Set initial value for electron density 
      %.. O+(2P) Calculate and print densities, production, loss
      PSEC=context.PEPION(1,3);           %.. Photoelectron production
      TPROD3=context.EUVION(1,3)+PSEC;    %.. Add EUV and photoelectrons
      OP2P = IRI2012.COP2P(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,TPROD3,PSEC, ...
        HEPLUS,N4S,NNO,TE);

      %.. O+(2D) Calculate and print densities, production, loss
      PSEC=context.PEPION(1,2);           %.. Photoelectron production
      TPROD2=context.EUVION(1,2);         %.. EUV
      OP2D = IRI2012.COP2D(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,TPROD2,OP2P, ...
        HEPLUS,N4S,NNO,PSEC);

      %.. O+(4S) Calculate and print densities, production, loss. 
      TPROD1=context.EUVION(1,1);
      PDISOP=context.EUVION(2,4) ...
            +context.EUVION(2,5) ...
            +context.PEPION(2,4) ...
            +context.PEPION(2,5);
%        write(*,*) JPRINT,K,ALT
      OXPLUS = IRI2012.COP4S(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,TPROD1,OP2D, ...
        OP2P,context.PEPION(1,1),PDISOP,N2PLUS,N2D,NNO,1.0,HEPLUS);
%        write(*,*) OXPLUS,TPROD1,OP2D
%     >    ,OP2P,context.PEPION(1,1),PDISOP,N2PLUS,N2D,NNO,HEPLUS

      N2A = IRI2012.CN2A(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,N2APRD,0.0, ...
         0.0,0.0);

      %..CALL CN2P(JPRINT,0,K,ALT,RTS,OXN,O2N,N2N,NE,PN2P,LN2P
      %..> ,N2P,P3X7,UVDISN,O2PLUS,NNO,N2PLUS)

      %.. N(2D) Calculate and print densities, production, loss. 
      [PN2D,LN2D] = IRI2012.CN2D(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NOPLUS,NE, ...
        N2PLUS,DISN2D,UVDISN,NPLUS,N2P,N2D,OXPLUS,NNO,N2A);
      N2D=PN2D/LN2D;

      %.. N2+ Calculate and print densities, production, loss. 
      N2PLUS = IRI2012.CN2PLS(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE, ...
        context.EUVION(3,1), ...
        context.EUVION(3,2), ...
        context.EUVION(3,3), ...
        context.PEPION(3,1), ...
        context.PEPION(3,2), ...
        context.PEPION(3,3), ...
        OP2D,OP2P,HEPLUS,NPLUS,NNO,N4S);

      %.. N+ Calculate and print densities, production, loss. 
      %.. Note that N(2D) is turned off in N+ solution 
      PHOTN=context.OTHPR2(3);  %.. N+ prod
      if USER_NPLUS > 0
        NPLUS=USER_NPLUS;  %.. User specified N+
      else
        NPLUS = IRI2012.CNPLS(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,DISNP, ...
          OXPLUS,N2D,OP2P,HEPLUS,PHOTN,O2PLUS,N4S,OP2D,N2PLUS,NNO);
      end

      %[PN4S,LN4S] = CN4S(JPRINT,28,K,ALT,RTS,OXN,O2N,N2N,NE,N4S, ...
      %  DISN4S,N2D,N2P,OXPLUS,N2PLUS,UVDISN,NOPLUS,NPLUS,NNO, ...
      %  O2PLUS,PDNOSR,1.0);
      %N4S=PN4S/LN4S;

      %.. NO Calculate and print densities, production, loss.
      if USER_NO > 1.0
        NNO=USER_NO;  %.. substitute user specified value
      else
        [PNO,LNO] = IRI2012.CNO(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE, ...
           N2D,N4S,N2P,NNO,O2PLUS,OXPLUS, ...
           context.OTHPR2(2),context.OTHPR2(1),N2A,NPLUS);
        if PNO == 0 && LNO == 0
          NNO = 0.0;
        else
          NNO=PNO/LNO;     %.. NO chemical equilibrium density
        end
        %.. Set a floor on NO density, which is needed below ~150 km at night 
        if NNO < 1.0E8*exp((100-ALT)/20)
          NNO=1.0E8*exp((100-ALT)/20);
        end
      end
      if NNO > 1.5E8
        NNO=1.5E8;      %.. Don't let NO get too big
      end

      %.. NO+ Calculate and print densities, production, loss. 
      [~,NOPLUS] = IRI2012.CNOP(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,OXPLUS, ...
         N2PLUS,O2PLUS,N4S,NNO,NPLUS,N2P,PLYNOP,VCON,N2D,OP2D);

      %.. O2+ Calculate and print densities, production, loss. 
      %.. EUV + PE production
      TPROD5=context.EUVION(2,1) ...
            +context.EUVION(2,2) ...
            +context.EUVION(2,3) ...
            +context.PEPION(2,1) ...
            +context.PEPION(2,2) ...
            +context.PEPION(2,3);
      [~,O2PLUS] = IRI2012.CO2P(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE, ...
        TPROD5,OXPLUS,OP2D,N2PLUS,NPLUS,N4S,NNO,OP2P);
    end

    %.. This section for chemical equilibrium densities for all species 
    %.. including O+. It is used when the Newton procedure fails to get O+
    %.. Don't bother if molecular ions greater than Ne/2
    %.. It increments ALTCHEM to force this action. The ion densities are 
    %.. normalized to the input NE. N+ is excluded in case it is user specified
    INEWT=1;
    SUMIONS=OXPLUS+NOPLUS+O2PLUS+NPLUS;
    if ALT < ALTCHEM || NOPLUS+O2PLUS > 0.85*NE
       OXPLUS=OXPLUS*NE/SUMIONS;
       NOPLUS=NOPLUS*NE/SUMIONS;
       O2PLUS=O2PLUS*NE/SUMIONS;
       NPLUS=NPLUS*NE/SUMIONS;
       INEWT=0;
       return;
    end

    %.. Newton solution for O+ density given the electron density (NE)
    %.. Go through twice to set up the derivative (F(x+h)-F(x))/h
    %.. First O+ guess for Newton. This is important for high altitudes
    %.. because Newton may converge on first try.
    %OXPLUSAVE=OXPLUS;
    if NE-NOPLUS-O2PLUS > 100
      OXPLUS=NE-NOPLUS-O2PLUS;
    end
    %..IF(SZAD.GT.89) OXPLUS=NE  %.. first guess at night    
    while true
      for ITS=1:2
        if ITS == 1
          H=OXPLUS*0.0001;         %.. increment for dn/dt
        end
        if ITS == 2
          OXPLUS=OXPLUS+H;       %.. increment N
        end

        %.. N+ Calculate and print densities, production, loss. 
        if USER_NPLUS > 0
          NPLUS=USER_NPLUS;  %.. User specified N+
        else
          NPLUS = IRI2012.CNPLS(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE,DISNP, ...
            OXPLUS,N2D,OP2P,HEPLUS,PHOTN,O2PLUS,N4S,OP2D,N2PLUS,NNO);
        end

        %.. N2+ Calculate and print densities, production, loss. 
        N2PLUS = IRI2012.CN2PLS(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE, ...
          context.EUVION(3,1), ...
          context.EUVION(3,2), ...
          context.EUVION(3,3), ...
          context.PEPION(3,1), ...
          context.PEPION(3,2), ...
          context.PEPION(3,3), ...
          OP2D,OP2P,HEPLUS,NPLUS,NNO,N4S);

        %.. NO+ Calculate and print densities, production, loss. 
        [~,NOPLUS] = IRI2012.CNOP(JPRINT,11,K,ALT,RTS,OXN,O2N,N2N,NE,OXPLUS, ...
           N2PLUS,O2PLUS,N4S,NNO,NPLUS,N2P,PLYNOP,VCON,N2D,OP2D);

        %.. O2+ Calculate and print densities, production, loss. 
        %.. EUV + PE production
        TPROD5 = context.EUVION(2,1) ...
                +context.EUVION(2,2) ...
                +context.EUVION(2,3) ...
                +context.PEPION(2,1) ...
                +context.PEPION(2,2) ...
                +context.PEPION(2,3);
        [~,O2PLUS] = IRI2012.CO2P(JPRINT,context.KONSOL,K,ALT,RTS,OXN,O2N,N2N,NE, ...
          TPROD5,OXPLUS,OP2D,N2PLUS,NPLUS,N4S,NNO,OP2P);

        %.. N(2D) Calculate and print densities, production, loss. 
        [PN2D,LN2D] = IRI2012.CN2D(JPRINT,16,K,ALT,RTS,OXN,O2N,N2N,NOPLUS,NE, ...
           N2PLUS,DISN2D,UVDISN,NPLUS,N2P,N2D,OXPLUS,NNO,N2A);
        N2D=PN2D/LN2D;

        %.. calculation of F(x) from the total ion concentration
        FEX(ITS)=OXPLUS+NOPLUS+O2PLUS+N2PLUS+NPLUS-NE;
      end

      %.. Test for convergence and add increment to O+ if not
      JITER=JITER+1;          %.. for stopping the iterations
      DEX=(FEX(2)-FEX(1))/H;
      OXPLUS=OXPLUS-H-FEX(1)/DEX;

      %.. If Newton fails, go back and calculate O+ chem equilibrium
      notgood = false;
      if JITER > 6 || OXPLUS < 0.0 || OXPLUS > 1.0E7
        ALTCHEM=ALT+1;  %.. forces chemical equilibrium densities
        notgood = true;
        break;         %.. Go back to chemical eqilibrium
      end

      %.. Test for convergence
      SUMIONS=OXPLUS+NOPLUS+O2PLUS+N2PLUS+NPLUS;
      if abs((NE-SUMIONS)/NE) <= 0.05
        break;
      end
    end
    if notgood
      continue;
    end

    %.. Normalize ion densities to the input electron density
    OXPLUS=OXPLUS*NE/SUMIONS;
    NOPLUS=NOPLUS*NE/SUMIONS;
    O2PLUS=O2PLUS*NE/SUMIONS;
    NPLUS=NPLUS*NE/SUMIONS;

    %.. If O+ a very minor ion, Newton solution may not be good, force  
    %.. chemical equilibrium solution instead
    if OXPLUS/SUMIONS < 0.1
      ALTCHEM=ALT+1;  %.. forces chemical equilibrium densities
      %.. Go back to chemical eqilibrium
    end
  end

end

