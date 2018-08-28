function [ PEFLUX,IMAX,AFAC,DE,EV ] = FLXCAL( context,IDIM,ALT,SZADEG,TE,TN,XN,XNE,XN2D,XOP2D,IMAX )
%FLXCAL evaluates the photoelectron flux
%:::::::::::::::::::::::::: PHOTOELECTRON MODEL  ::::::::::::::::::::::::
%....... This subroutine evaluates the photoelectron flux using the concept
%.......  production frequencies developed by Phil Richards at Utah 
%....... State University March 1984. It supercedes the model described in
%....... JGR, p2155, 1983. Contact EAST::CSPARA::RICHARDS on SPAN network
%------- Some minor updates in April 1992 indicated by C----
%....... I would appreciate any feedback on bugs or clarity and if it 
%....... contributes substantially to a paper, I would appreciate the 
%....... appropriate acknowledgement.
%......       **************** WARNING ****************
%...... This program is constructed to produce reasonable agreement with
%...... the Atmosphere Explorer-E PES fluxes of John Doering (Lee et al.
%...... PSS 1980, page 947). It will NOT give good fluxes if the EUV 
%...... attenuation is greater than about a factor of 7 (AFAC < 0.14).
%...... The model accurately reproduces the measured fluxes very closely
%...... for the case in the test driver at 148 km SZA=53 when AFAC=0.19.
%...... You should compare the output against the Lee et al. 1980 fluxes
%...... periodically as a check. It is doubtful below 140km during the
%...... day and below 200km near sunset. Between 200km & 350km, it should
%...... be good for solar zenith angles < 90 degrees. Above 350 km there
%...... is considerable uncertainty due to neglect of transport but most
%...... models have similar uncertainties at high altitudes due to the 
%...... uncertainty in the conjugate photoelectron flux, and the pitch 
%...... angle distribution.
%
%------ ALT = altitude (km)  { 120 -> 500 }
%------ SZADEG = solar zenith angle  {0 -> 90 degrees ? }
%------ TE, TN = electron, neutral temperatures (K)
%------ XN, XNE = O, O2, N2, and electron densities  (cm-3)
%------ XN2D, XOP2D = N(2D) and O+(2D) densities for electron quenching
%------ (cm-3). You may put these to ZERO if not available.
%------ PEFLUX = photoelectron flux to be returned (eV cm2 sec)-1
%------ AFAC = the solar EUV attenuation warning flag
%      SUBROUTINE FLXCAL(IDIM,ALT,SZADEG,
%        TE,TN,XN,XNE,XN2D,XOP2D,PEFLUX,AFAC,IMAX,DE,EV)

%      INTEGER RDIM,IMAX
%      REAL EMAX             !.. maximum photoelectron energy.
%      PARAMETER (RDIM=84)
%c      REAL RJOX(RDIM),RJN2(RDIM),XN(3),COLUM(3),PEFLUX(IDIM)
%c     >  ,DE(RDIM),DELTE(RDIM),EV(RDIM),EN(RDIM),UVFAC,EUV
%c      COMMON/SOL/UVFAC(59),EUV
%      REAL RJOX(RDIM),RJN2(RDIM),XN(3),COLUM(3),PEFLUX(IDIM)
%     >  ,DE(RDIM),DELTE(RDIM),EV(RDIM),EN(RDIM),UVFAC(59),EUV
%      COMMON/SOL/UVFAC,EUV
  persistent RDIM RJOX RJN2 EN DELTE EMAX;
  if isempty(RDIM)
    RDIM = 84;
      %.. photoelectron production frequencies by 1.0E9. Renormalized below
      %-- O production frequencies
    RJOX = [19,19,19,19,19,19,19,19,19,19,15,18,14,10,13,9,13,9,7,11,6,26,6,31,6,5,22,4, ...
         4,5,4.04,2.56,1.9,2.28,2.12,0.96,0.24,0.14,0.14,0.1,0.1,0.1 ...
         ,0.1,0.1,.05,.05,.05,.05,.05,.05,.05,.05,.05,.05,.04,.04,.04,.04, ...
             .04,.04,.04,.04,.04,.04,.01,.01,.01,.01,.01,.01,.01,.01,.01, ...
             .01,.02,.02,.02,.02,.02,.02,.02,.02,.02,.02];

      %-- N2 production frequencies
    RJN2 = [40,40,40,40,40,40,43,35,35,28,29,21,25,19,19,13,19,16,12,11,7,18,8, ...
        46,27,5,5,5,5,5,5.34,2.92,1.84,2.22,1.62,0.62,0.05,0.05,0.05, ...
        0.05,0.05,0.05,0.05,0.044,.02,.02,.02,.02,.02,.02,.02,.02,.02,.02, ...
             .01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01, ...
             .01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01,.01];

      %-- PE energy grid
    EN = [0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5, ...
        13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,21.5,22.5,23.5,24.5, ...
        25.5,26.5,27.5,28.5,29.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5, ...
        67.5,72.5,77.5,82.5,87.5,92.5,97.5,105,115,125,135,145,155, ...
        165,175,185,195,205,215,225,235,245,255,265,275,285,295,305, ...
        315,325,335,345,355,365,375,385,395,405,415,425,435,445,455, ...
        465,475,485,495];

      %-- PE energy steps
    DELTE = [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0, ...
             1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0, ...
             5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,10, ...
             10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, ...
             10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10];
	  EMAX = 286.0;          %..  Maximum PE energy
  end
  PEFLUX = zeros(IDIM,1);
  DE = zeros(RDIM,1);
  EV = zeros(RDIM,1);
  SZA = SZADEG*IRI2012.UMR;   %.. convert solar zenith angle to radians

  if IMAX < 10
    %.. transfer energy grid to pass back
    for IE=1:RDIM
      if EN(IE) < EMAX
        IMAX=IE;
      end
      DE(IE)=DELTE(IE);
      EV(IE)=EN(IE);
    end
  end

  %.. 2.5eV production from electron quenching of N2D
  PN2D=XN2D*XNE*6.0E-10*sqrt(TE/300.0);
  %.. 3.3eV production from electron quenching of O+(2D)
  POP2D=XOP2D*XNE*6.6E-8*sqrt(300./TE);

  %CASEL=0.0;

  %.. evaluate the neutral column density  &&&&&&&&
  I = 0;
  COLUM = IRI2012.SCOLUM(I,SZA,ALT*1.0E5,TN,XN);
  SIGOX = 0.0;
  SIGN2 = 0.0;
  SIGEE = 0.0;

  %...... begin flux calculation loop............................
  for IE=1:IMAX
    I=IMAX+1-IE;
    if I < 1
      break;
    end
    PEFLUX(I)=0.0;

    %... evaluate energy of photon responsible for electron at energy EE
    EE=EV(I);
    EP=EE+17;
    if EE < 22
      EP=45;
    end
    if EE >= 22 && EE < 28
      EP=41;
    end
    if EE >= 28 && EE < 38
      EP=49;
    end

    %.. evaluate total photoionization cross sections for photon energy EP
    XSOXT=IRI2012.T_XS_OX(EP);         %.. New OX cross section
    XSO2T=2.2*IRI2012.T_XS_OX(EP);     %.. O2 XS is 2.2* O XS
    XSN2T=IRI2012.T_XS_N2(EP);         %.. New N2 cross section

    %... evaluate EUV attenuation factor AFAC
    TAU=COLUM(1)*XSOXT+COLUM(2)*XSO2T+COLUM(3)*XSN2T;
    AFAC=exp(-TAU);

    %..... low energy cascade production from O(1D) and N2* impact
    CASOX=0.0;
    if EE < 10
      CASOX=PEFLUX(I+2)*SIGOX*XN(1);
    end
    CASN2=0.0;
    if EE < 6
      CASN2=PEFLUX(I+1)*SIGN2*XN(3);
    end

    %..... cascade production from thermal electron degradation
    CASEL=0.0;
    if I < IMAX
      CASEL=PEFLUX(I+1)*SIGEE*XNE;
    end

    %... Production from electron quenching of metastables
    EPN2D=0.0;
    if floor(EE) == 3
      EPN2D=PN2D;
    end
    EPOP2D=0.0;
    if floor(EE) == 4
      EPOP2D=POP2D;
    end

    %.... evaluate cross sections (must be after cascade production)
    [SIGOX,SIGN2,SIGEE] = IRI2012.SIGEXS(EE,TE,XNE);

    %..... adjust EUV production rate for different period of solar cycle
    FFAC = IRI2012.FACFLX(EE,context.UVFAC);

    %..... Production of pe's at energy EE, taking into account
    %..... attenuation and EUV variation, and renormalize frequencies

    PRODOX=RJOX(I)*XN(1)*AFAC*FFAC*1.0E-9;
    PRODN2=RJN2(I)*XN(3)*AFAC*FFAC*1.0E-9;

    %..... Sum all the production rates
    PROD=PRODOX+PRODN2+CASEL+CASOX+CASN2+EPN2D+EPOP2D;

    %.... total loss through collisions
    RLOSS=SIGOX*XN(1)+SIGN2*XN(3)+SIGEE*XNE;

    %....... evaluate photoelectron flux
    if PROD == 0 && RLOSS == 0
      PEFLUX(I) = 0.0;
    elseif RLOSS == 0
      if PROD >= 0
        PEFLUX(I) = 1.0e30;
      else
        PEFLUX(I) = -1.0e30;
      end
    else
      PEFLUX(I)=PROD/RLOSS;
    end
  end

end

