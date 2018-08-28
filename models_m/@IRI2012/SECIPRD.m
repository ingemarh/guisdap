function [ N2APRD, context ] = SECIPRD( context,ALT,SZADEG,F107,F107A,TE,TN, ...
  OXN,O2N,N2N,XNE )
%SECIPRD test driver demonstrates how to call the model
%
%.........................<PESIMP.FOR>......................3 APR 92
%...... This test driver demonstrates how to call the model and 
%...... how to calculate electron heating rate and 3371 excitation rate.
%      SUBROUTINE SECIPRD(ALT,SZADEG,F107,F107A,
%          TE,TN,OXN,O2N,N2N,XNE,N2APRD)

%      INTEGER I,K,IK           !-- loop control variables
%      INTEGER IDIM             !.. Array dimensions
%      INTEGER IMAX             !.. Maximum PE energy
%      PARAMETER (IDIM=501)
%      REAL PEFLUX(IDIM)        !-- PE flux
%      REAL SIGIT(3)            !-- Total electron impact cross sections
%      REAL SIGEX(22)           !-- Electron impact cross sections for OX
%      REAL ALT                 !-- ALT = altitude (km)  { 120 -> 500 }
%      REAL SZADEG              !-- solar zenith angle {0 -> 90 degrees}
%      REAL F107, F107A         !-- F107 = Solar 10.7 cm flux
%      REAL TE,TN               !-- electron, neutral temperatures (K)
%      REAL XN(3),OXN,O2N,N2N   !-- XN, O, O2, N2, neutral densities  (cm-3)
%      REAL XNE                 !-- electron density  (cm-3)
%      REAL XN2D                !-- N(2D) density for N(2D) + e -> 2.5 eV
%      REAL XOP2D               !-- O+(2D) density for O+(2D) + e -> 3.3 eV
%      REAL SPRD(3,6)
%      REAL SIGOX,SIGN2,SIGEE  !.. Total exciation cross sections for O, N2, O2
%      REAL N2APRD             !.. Production of N2A
%      REAL DE(IDIM),EV(IDIM)
%.. various ionization and excitation rates by EUV and PE
%      REAL EUVION,PEXCIT,PEPION,OTHPR1,OTHPR2
%      COMMON/EUVPRD/EUVION(3,12),PEXCIT(3,12),PEPION(3,12),OTHPR1(6)
%     >   ,OTHPR2(6)
  persistent SPRD IDIM;
  if isempty(SPRD)
    SPRD = transpose([.4,.56,.44; ...
                      .4,.28,.44; ...
                      .2,.06,.10; ...
                      0.,.05,.00; ...
                      0.,.05,.00; ...
                      0.0,0.0,0.02]);
    IDIM=501;
  end
  IMAX = 0;              %.. Initialize IMAX Reset in FLXCAL
  XN = zeros(3,1);
      %.. Transfer neutral densities to the density array
  XN(1)=OXN;
  XN(2)=O2N;
  XN(3)=N2N;
  N2APRD=0.0;
  for K=1:3
    for IK=1:6
      context.PEPION(K,IK)=1.0E-15;
      context.PEXCIT(K,IK)=1.0E-15;
    end
  end

  %.. Cannot calculate PE if no densities
  if OXN < 1.0E5 || N2N < 1.0E5
    return;
  end
  if SZADEG > 105
    return;
  end

  %********************************************************************
  %.. Go and get the photoelectron fluxes
   XN2D=0;        %.. N(2D) density for calculating N(2D) + e -> 2.5 eV
   XOP2D=0;       %.. O+(2D) density for calculating O+(2D) + e -> 3.3 eV
   [PEFLUX,IMAX,~,DE,EV] = context.FLXCAL(IDIM,ALT,SZADEG, ...
     TE,TN,XN,XNE,XN2D,XOP2D,IMAX);
  %***************************************************************

  %........ sample calculation of ion production rates. 
  for I=1:IMAX
    E=EV(I);
    SIGIT = IRI2012.TXSION(E);                     %.. total ion XS
    [~,SIGN2,~] = IRI2012.SIGEXS(E,TE,XNE);  %.. Total excitation XS
    [SIGEX,~] = IRI2012.OXSIGS(E);               %.. OX cross sections

    if E < 250
      N2APRD=N2APRD+0.22*PEFLUX(I)*SIGN2*XN(3)*DE(I); %.. N2(A) prod
    end
    context.PEXCIT(1,1)=context.PEXCIT(1,1)+PEFLUX(I)*SIGEX(1)*XN(1)*DE(I);   %.. O(1D) prod
    context.PEXCIT(1,2)=context.PEXCIT(1,2)+PEFLUX(I)*SIGEX(2)*XN(1)*DE(I);   %.. O(1S) prod

    %.. Evaluate ionization branching ratios for O+
    [SPRD(1,1),SPRD(1,2),SPRD(1,3)] = IRI2012.OXRAT(E);

    %.. Calculate ion production rates
    for K=1:3
      for IK=1:6
        context.PEPION(K,IK)=context.PEPION(K,IK)+PEFLUX(I)*SIGIT(K)*XN(K)*SPRD(K,IK)* ...
          DE(I);
      end
    end

%      EP=E+17;
%      PEFLX=PEFLUX(I)/12.57;
%      WRITE(29,'(3F8.1,1P,22E10.2)') ALT,E,12398/EP,PEFLX,
%     >     PEFLUX(I),(SIGIT(K),K=1,3),T_XS_OX(EP),2.2*T_XS_OX(EP),
%     >     T_XS_N2(EP),PEPION(1,1),PEXCIT(1,1)
  end

end

