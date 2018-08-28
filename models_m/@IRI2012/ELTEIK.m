function [ TE,SIGTE,INVDIP ] = ELTEIK( CRD,PF107Y,INVDIP,FL,DIMO,B0,...
                                             DIPL,MLT,ALT,DDD,PF107 )
%ELTEIK Empirical model of electron temperature
%                     
%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                     
%^^^^^^^^* ELECTRON TEMPERATURE ^^^^^^^^^^                    
%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                     
%
%      SUBROUTINE ELTEIK(CRD,PF107Y,INVDIP,FL,DIMO,B0,
%     &                   DIPL,MLT,ALT,DDD,PF107,TE,SIGTE)
%----------------------------------------------------------------------
% Empirical model of electron temperature (Te) in the outer ionosphere
% with inclusion of solar activity.
% Based on spherical harmonics approximation of measured
% Te (all available satellites) at altitudes centred on 350km, 550km,
% 850km, 1400km, and 2000km. For intermediate altitudes a linear
% interpolation is used. Recommended altitude range: 300-2500 km!!!
% Linear extrapolation is used for altitude ranges <300;350)km
% and (2000;2500> km. For days between seasons centred at
% (21.3. = 79; 21.6. = 171; 23.9. 265; 21.12. = 354) Te is
% interpolated by a harmonic function.
% Inputs: CRD - 0 .. INVDIP
%               1 .. FL, DIMO, B0, DIPL (used for calc. INVDIP inside)
%         PF107Y - 0 .. PF107 correction NOT included
%                  1 .. PF107 correction included
%         INVDIP - "mix" coordinate of the dip latitude and of
%                    the invariant latitude;
%                    positive northward, in deg, range <-90.0;90.0>
%         FL, DIMO, BO - McIlwain L parameter, dipole moment in
%                        Gauss, magnetic field strength in Gauss -
%                        parameters needed for invariant latitude
%                        calculation
%         DIPL - dip latitude
%                positive northward, in deg, range <-90.0;90.0>
%         MLT - magnetic local time (central dipole)
%               in hours, range <0;24)
%         ALT - altitude above the Earth's surface;
%               in km, range <500;3000>
%         DDD - day of year; range <0;365>
%         PF107 - Phil Richard's solar radio flux;
% Output: TE - electron temperature in K
%         SIGTE - standard deviation (or model error) of TE in K
% Versions: 1.00 (IDL) the first version Te=Te(invl,mlt,alt,season)
%           1.50 (IDL) corrected IK19 Te at 900km for possible Ne>2E11 m-3
%           2.00 (IDL) F107 included as a linear perturbation on global Te pattern
%                      Te=Te(invlat,mlt,alt,season,F107)
%           3.00 (IDL) invdipl introduced
%           2000 (IDL,FORTRAN) correction for seasons included
%           2010 (IDL,FORTRAN) completely new version 
% Authors of the model (v 2011)
%                V. Truhlik, D. Bilitza, and L. Triskova
% Author of the code:
%         Vladimir Truhlik
%         Institute of Atm. Phys.
%         Bocni II.
%         141 31 Praha 4, Sporilov
%         Czech Republic
%         e-mail: vtr@ufa.cas.cz
%----------------------------------------------------------------------

%      REAL INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,PF107,TE,SIGTE
%      INTEGER CRD,PF107Y,DDD,SEZDAY,XDAY
%      INTEGER MIRREQ(81)
%      REAL D(5,3,81),DERRTE(5,3,81),DPF107(5,3,81)
%      DOUBLE PRECISION B(8),A
%      REAL DPI,DTOR,ASA,INVL,RINVL,INVDP,RDIPL,ALFA,BETA
%      REAL RMLT,RCOLAT
%      REAL C(82)
%      INTEGER SEZA,SEZB,DDDA,DDDB,DDDD
%      REAL T350,T350A,T350B,T550,T550A,T550B,T850,T850A,T850B,
%     &     T1400,T1400A,T1400B,T2000,T2000A,T2000B 
%      REAL P350A,P350B,P550A,P550B,P850A,P850B,
%     &     P1400A,P1400B,P2000A,P2000B 
%      REAL E350,E350A,E350B,E550,E550A,E550B,E850,E850A,E850B,
%     &     E1400,E1400A,E1400B,E2000,E2000A,E2000B 
%      REAL TP350A,TP350B,TP550A,TP550B,TP850A,TP850B,
%     &     TP140A,TP140B,TP200A,TP200B
%      INTEGER FUN
%      INTEGER I
  persistent NI NL NM D DERRTE DPF107;
  if isempty(NI)
    NI = 81;
    NL = 8;
    NM = 8;
%////////////////////////////////coefficients - main model part/////////////////
    MIRREQ = [...
        1,-1, 1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, ...
        1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1,-1, 1, 1,-1, 1, ...
       -1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1, 1,-1, ...
        1,-1, 1,-1, 1,-1, 1,-1, 1, 1,-1, 1, 1,-1, 1,-1, 1, 1];
    D = IRI2012.KOEFD(MIRREQ);
    DERRTE = IRI2012.KODERR(MIRREQ);
    DPF107 = IRI2012.KOF107(MIRREQ);
  end
  %//////////////////////thresholds of solar activity///////////////////////////
  if PF107 > 250
    PF107=250;
  end
  if PF107 < 80
    PF107=80;
  end
  %/////////////////////////////////////////////////////////////////////////////
  if CRD == 1
    INVDP=IRI2012.INVDPC(FL,DIMO,B0,DIPL,IRI2012.UMR);
  elseif CRD == 0
    INVDP=INVDIP;
  else
    TE = 0.0;
    SIGTE = 0.0;
    return;
  end
  RMLT=MLT*IRI2012.UMR*15.0;
  RCOLAT=(90.0-INVDP)*IRI2012.UMR;
  C = IRI2012.SPHARM_IK(NL,NM,RCOLAT,RMLT);
  if (DDD >= 79) && (DDD < 171)
  %     21.3. - 20.6.
    SEZA=1;
    SEZB=2;
    DDDA=79;
    DDDB=171;
    DDDD=double(DDD);
    FUN=0;
  elseif (DDD >= 171) && (DDD < 265)
  %     21.6. - 22.9.
    SEZA=2;
    SEZB=1;
    DDDA=171;
    DDDB=265;
    DDDD=floor(double(DDD));
    FUN=1;
  elseif (DDD >= 265) && (DDD < 354)
  %     23.9. - 20.12.
    SEZA=1;
    SEZB=3;
    DDDA=265;
    DDDB=354;
    DDDD=floor(double(DDD));
    FUN=0;
  else %if (DDD >= 354) || (DDD < 79)
  %     21.12. - 20.3.
    SEZA=3;
    SEZB=1;
    DDDA=354;
    DDDB=365+79;
    %DDDD=DDD;
    if DDD >= 354
      DDDD=floor(double(DDD));
    else
      DDDD=floor(double(DDD))+365;
    end
    FUN=1;
  end
  %     model Te
  T350A=0.0;
  T350B=0.0;
  T550A=0.0;
  T550B=0.0;
  T850A=0.0;
  T850B=0.0;
  T1400A=0.0;
  T1400B=0.0;
  T2000A=0.0;
  T2000B=0.0;
  for I=1:NI
    T350A=T350A+C(I)*D(1,SEZA,I);
    T350B=T350B+C(I)*D(1,SEZB,I);
    T550A=T550A+C(I)*D(2,SEZA,I);
    T550B=T550B+C(I)*D(2,SEZB,I);
    T850A=T850A+C(I)*D(3,SEZA,I);
    T850B=T850B+C(I)*D(3,SEZB,I);
    T1400A=T1400A+C(I)*D(4,SEZA,I);
    T1400B=T1400B+C(I)*D(4,SEZB,I);
    T2000A=T2000A+C(I)*D(5,SEZA,I);
    T2000B=T2000B+C(I)*D(5,SEZB,I);
  end
  T350A=10.^T350A;
  T350B=10.^T350B;
  T550A=10.^T550A;
  T550B=10.^T550B;
  T850A=10.^T850A;
  T850B=10.^T850B;
  T1400A=10.^T1400A;
  T1400B=10.^T1400B;
  T2000A=10.^T2000A;
  T2000B=10.^T2000B;
  %     model PF107
  P350A=0.0;
  P350B=0.0;
  P550A=0.0;
  P550B=0.0;
  P850A=0.0;
  P850B=0.0;
  P1400A=0.0;
  P1400B=0.0;
  P2000A=0.0;
  P2000B=0.0;
  for I=1:NI
    P350A=P350A+C(I)*DPF107(1,SEZA,I);
    P350B=P350B+C(I)*DPF107(1,SEZB,I);
    P550A=P550A+C(I)*DPF107(2,SEZA,I);
    P550B=P550B+C(I)*DPF107(2,SEZB,I);
    P850A=P850A+C(I)*DPF107(3,SEZA,I);
    P850B=P850B+C(I)*DPF107(3,SEZB,I);
    P1400A=P1400A+C(I)*DPF107(4,SEZA,I);
    P1400B=P1400B+C(I)*DPF107(4,SEZB,I);
    P2000A=P2000A+C(I)*DPF107(5,SEZA,I);
    P2000B=P2000B+C(I)*DPF107(5,SEZB,I);
  end
  P350A=10.^P350A;
  P350B=10.^P350B;
  P550A=10.^P550A;
  P550B=10.^P550B;
  P850A=10.^P850A;
  P850B=10.^P850B;
  P1400A=10.^P1400A;
  P1400B=10.^P1400B;
  P2000A=10.^P2000A;
  P2000B=10.^P2000B;
  %     model errTe
  E350A=0.0;
  E350B=0.0;
  E550A=0.0;
  E550B=0.0;
  E850A=0.0;
  E850B=0.0;
  E1400A=0.0;
  E1400B=0.0;
  E2000A=0.0;
  E2000B=0.0;
  for I=1:NI
    E350A=E350A+C(I)*DERRTE(1,SEZA,I);
    E350B=E350B+C(I)*DERRTE(1,SEZB,I);
    E550A=E550A+C(I)*DERRTE(2,SEZA,I);
    E550B=E550B+C(I)*DERRTE(2,SEZB,I);
    E850A=E850A+C(I)*DERRTE(3,SEZA,I);
    E850B=E850B+C(I)*DERRTE(3,SEZB,I);
    E1400A=E1400A+C(I)*DERRTE(4,SEZA,I);
    E1400B=E1400B+C(I)*DERRTE(4,SEZB,I);
    E2000A=E2000A+C(I)*DERRTE(5,SEZA,I);
    E2000B=E2000B+C(I)*DERRTE(5,SEZB,I);
  end
  E350A=10.^E350A;
  E350B=10.^E350B;
  E550A=10.^E550A;
  E550B=10.^E550B;
  E850A=10.^E850A;
  E850B=10.^E850B;
  E1400A=10.^E1400A;
  E1400B=10.^E1400B;
  E2000A=10.^E2000A;
  E2000B=10.^E2000B;
  %
  if PF107Y == 1
    [TP350A,TP350B,TP550A,TP550B,TP850A,TP850B, ...
      TP140A,TP140B,TP200A,TP200B] = IRI2012.TPCORR(INVDP,MLT,DDD,PF107, ...
      P350A,P350B,P550A,P550B,P850A,P850B, ...
      P1400A,P1400B,P2000A,P2000B);
    T350A=T350A+TP350A;
    T350B=T350B+TP350B;
    T550A=T550A+TP550A;
    T550B=T550B+TP550B;
    T850A=T850A+TP850A;
    T850B=T850B+TP850B;
    T1400A=T1400A+TP140A;
    T1400B=T1400B+TP140B;
    T2000A=T2000A+TP200A;
    T2000B=T2000B+TP200B;
  end
  %     Te
  if FUN == 0
    SEZDAY=(DDDB-DDDA);
    XDAY=DDDD-DDDA;
    T350=(T350B-T350A)*sin(IRI2012.pi/2.0*floor(XDAY/SEZDAY))+T350A;
    T550=(T550B-T550A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+T550A;
    T850=(T850B-T850A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+T850A;
    T1400=(T1400B-T1400A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+T1400A;
    T2000=(T2000B-T2000A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+T2000A;
  else
    SEZDAY=(DDDB-DDDA);
    XDAY=DDDD-DDDA;
    T350=(T350A-T350B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+T350B;
    T550=(T550A-T550B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+T550B;
    T850=(T850A-T850B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+T850B;
    T1400=(T1400A-T1400B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+T1400B;
    T2000=(T2000A-T2000B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+T2000B;
  end
  %     error Te
  if FUN == 0
    SEZDAY=(DDDB-DDDA);
    XDAY=DDDD-DDDA;
    E350=(E350B-E350A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+E350A;
    E550=(E550B-E550A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+E550A;
    E850=(E850B-E850A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+E850A;
    E1400=(E1400B-E1400A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+E1400A;
    E2000=(E2000B-E2000A)*sin(IRI2012.pi/2.0*XDAY/SEZDAY)+E2000A;
  else
    SEZDAY=(DDDB-DDDA);
    XDAY=DDDD-DDDA;
    E350=(E350A-E350B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+E350B;
    E550=(E550A-E550B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+E550B;
    E850=(E850A-E850B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+E850B;
    E1400=(E1400A-E1400B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+E1400B;
    E2000=(E2000A-E2000B)*cos(IRI2012.pi/2.0*XDAY/SEZDAY)+E2000B;
  end
  % ////////////////////////////////////////////////////////
  %     Te linear interpolation for altitude
  if ALT < 550
    TE=(T550-T350)/200.0*(ALT-350)+T350;
    SIGTE=(E550-E350)/200.0*(ALT-350)+E350;
  elseif (ALT < 850) % (ALT >= 550) && 
    TE=(T850-T550)/300.0*(ALT-550)+T550;
    SIGTE=(E850-E550)/300.0*(ALT-550)+E550;
  elseif (ALT < 1400) % (ALT >= 850) && 
    TE=(T1400-T850)/550.0*(ALT-850)+T850;
    SIGTE=(E1400-E850)/550.0*(ALT-850)+E850;
  else % if ALT >= 1400
    TE=(T2000-T1400)/600.0*(ALT-1400)+T1400;
    SIGTE=(E2000-E1400)/600.0*(ALT-1400)+E1400;
  end

  INVDIP=INVDP;

end

