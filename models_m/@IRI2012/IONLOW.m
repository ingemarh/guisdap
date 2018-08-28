function [ NION, D ] = IONLOW( CRD,INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,DDD,D,ION )
%IONLOW calculates relative density of O+, H+, He+ or N+
%
%      SUBROUTINE IONLOW(CRD,INVDIP,FL,DIMO,B0,
%                         DIPL,MLT,ALT,DDD,D,ION,NION)
%---------------------------------------------------------------------------
% IONLOW calculates relative density of O+, H+, He+ or N+  in the outer
% ionosphere for a low solar activity (F107 < 100).
% Based on spherical harmonics approximation of relative ion density
% (by AE-C, and AE-E) at altitudes centred on 400km, 650km, and 1000km.
% For intermediate altitudes an interpolation is used. 
% Recommended altitude range: 350-2000 km!!!
% For days between seasons centred at (21.3. = 79; 21.6. = 171;
% 23.9. 265; 21.12. = 354) relative ion density is linearly interpolated.
% Inputs: CRD - 0 .. INVDIP
%               1 .. FL, DIMO, B0, DIPL (used for calculation INVDIP inside)
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
%               in km, range <350;2000>
%         DDD - day of year; range <0;365>
%         D - coefficints of spherical harmonics for a given ion
%         ION - ion species (0...O+, 1...H+, 2...He+, 3...N+)
% Output: NION - relative density for a given ion 
%---------------------------------------------------------------------------

%      REAL INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,NION
%      INTEGER CRD,DDD,ION
%      DIMENSION  D(3,3,49),MIRREQ(49)
%      REAL INVDP,INVDPC,DTOR
%      REAL RMLT,RCOLAT
%      REAL C(49)
%      INTEGER SEZA,SEZB,SEZAI,SEZBI,DDDA,DDDB,DDDD
%      REAL N0A400,N0B400,N400A,N400B,N400
%      REAL N0A650,N0B650,N650A,N650B,N650
%      REAL N0A100,N0B100,N100A,N100B,N1000
%	REAL ANO(3),AH(3),DNO(1),ST(2)
%	COMMON/ARGEXP/ARGMAX
  persistent MIRREQ;
  if isempty(MIRREQ)
	  MIRREQ = [ ...
                  1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, ...
                  1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1, ...
                 -1, 1,-1, 1,-1, 1, 1,-1, 1, 1,-1, 1,-1, 1, 1];
%///////////////////////////////////////////////////////////////////////////////
  end
  ST = zeros(2,1);
%     coefficients for mirroring
  for I=1:49
    D(1,3,I)=D(1,2,I)*MIRREQ(I);
    D(2,3,I)=D(2,2,I)*MIRREQ(I);
    D(3,3,I)=D(3,2,I)*MIRREQ(I);
  end
  if CRD == 1
    INVDP=IRI2012.INVDPC(FL,DIMO,B0,DIPL,IRI2012.UMR);
  elseif CRD == 0
    INVDP=INVDIP;
  else
    return;
  end
  RMLT=MLT*IRI2012.UMR*15.0;
  RCOLAT=(90.0-INVDP)*IRI2012.UMR;
  C = IRI2012.SPHARM_IK(6,6,RCOLAT,RMLT);
%     21.3. - 20.6.
  if (DDD >= 79) && (DDD < 171)
    SEZA=1;
    SEZB=2;
    DDDA=79;
    DDDB=171;
    DDDD=floor(double(DDD));
  end
  %     21.6. - 22.9.
  if (DDD >= 171) && (DDD < 265)
    SEZA=2;
    SEZB=4;
    DDDA=171;
    DDDB=265;
    DDDD=floor(double(DDD));
  end
  %     23.9. - 20.12.
  if (DDD >= 265) && (DDD < 354)
    SEZA=4;
    SEZB=3;
    DDDA=265;
    DDDB=354;
    DDDD=floor(double(DDD));
  end
  %     21.12. - 20.3.
  if (DDD >= 354) || (DDD < 79)
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
  end
  SEZAI=mod(SEZA-1,3)+1;
  SEZBI=mod(SEZB-1,3)+1;
  %     400km level
  N0A400=0.0;
  N0B400=0.0;
  for I=1:49
    N0A400=N0A400+C(I)*D(1,SEZAI,I);
    N0B400=N0B400+C(I)*D(1,SEZBI,I);
  end
  N400A=N0A400;
  N400B=N0B400;
  N400=(N400B-N400A)/(DDDB-DDDA)*(DDDD-DDDA)+N400A;
%     650km level
  N0A650=0.0;
	N0B650=0.0;
	for I=1:49
    N0A650=N0A650+C(I)*D(2,SEZAI,I);
    N0B650=N0B650+C(I)*D(2,SEZBI,I);
  end
	N650A=N0A650;
	N650B=N0B650;
	N650=(N650B-N650A)/(DDDB-DDDA)*(DDDD-DDDA)+N650A;
%     1000km level
  N0A100=0.0;
  N0B100=0.0;
  for I=1:49
    N0A100=N0A100+C(I)*D(3,SEZAI,I);
    N0B100=N0B100+C(I)*D(3,SEZBI,I);
  end
	N100A=N0A100;
	N100B=N0B100;
	N1000=(N100B-N100A)/(DDDB-DDDA)*(DDDD-DDDA)+N100A;
          
  %      IF (ALT < 650) NO=(N650-N400)/250.0*(ALT-400)+N400
  %      IF (ALT >= 650) NO=(N1000-N650)/350.0*(ALT-650)+N650

  %      NION=10^NO

  %-02/07/09- n(O+) AND n(N+) must not increase above 650km
  if ((ION == IRI2012.O_ION) || (ION == IRI2012.N_ION)) && (N1000 > N650)
    N1000=N650;
  end
  %-02/07/09- n(H+) must not decrease above 650km
  if (ION == IRI2012.H_ION) && (N1000 < N650)
    N1000=N650;
  end

  ANO(1)=N400;
	ANO(2)=N650;
  ANO(3)=N1000;
	  
	AH(1)=400.;
  AH(2)=650.;
  AH(3)=1000.;

  DNO(1)=20.;

  ST1=(ANO(2)-ANO(1))/(AH(2)-AH(1));
  I=2;
  ST2=(ANO(I+1)-ANO(I))/(AH(I+1)-AH(I));
  ANO(I)=ANO(I)-(ST2-ST1)*DNO(I-1)*log(2.);

  for I=1:2
    ST(I)=(ANO(I+1)-ANO(I))/(AH(I+1)-AH(I));
  end

  %context.ARGMAX=88.0;
  SUM=ANO(1)+ST(1)*(ALT-AH(1));
     
  I=1;
	AA = IRI2012.EPTR(ALT  ,DNO(I),AH(I+1));
	BB = IRI2012.EPTR(AH(1),DNO(I),AH(I+1));
  SUM=SUM+(ST(I+1)-ST(I))*(AA-BB)*DNO(I);
                
  NION=10.^SUM;

end

