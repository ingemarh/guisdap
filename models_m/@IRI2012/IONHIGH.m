function [ NION, D ] = IONHIGH( CRD,INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,DDD,D,ION )
%IONHIGH calculates relative density of O+, H+, He+ or N+
%
%      SUBROUTINE IONHIGH(CRD,INVDIP,FL,DIMO,B0,
%     &                   DIPL,MLT,ALT,DDD,D,ION,NION)
%---------------------------------------------------------------------------
% IONHIGH calculates relative density of O+, H+, He+ or N+  in the outer
% ionosphere for high solar activity conditions (F107 >= 100).
% Based on spherical harmonics approximation of relative ion density
% (by IK24) at altitudes centred on 550km, 900km, 1500km, and 2250km.
% For intermediate altitudes a interpolation is used. 
% Recommended altitude range: 500-3000 km!!!
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
%               in km, range <500;3000>
%         DDD - day of year; range <0;365>
%         D - coefficints of spherical harmonics for a given ion
%         ION - ion species (0...O+, 1...H+, 2...He+, 3...N+)
% Output: NION - relative density for a given ion 
%---------------------------------------------------------------------------

%      REAL INVDIP,FL,DIMO,B0,DIPL,MLT,ALT,NION
%	INTEGER CRD,DDD,ION
%      DIMENSION  D(4,3,49),MIRREQ(49)
%      REAL INVDP,INVDPC,DTOR
%      REAL RMLT,RCOLAT
%      REAL C(49)
%      INTEGER SEZA,SEZB,SEZAI,SEZBI,DDDA,DDDB,DDDD
%      REAL N0A550,N0B550,N550A,N550B,N550
%      REAL N0A900,N0B900,N900A,N900B,N900
%      REAL N0A150,N0B150,N150A,N150B,N1500
%      REAL N0A250,N0B250,N250A,N250B,N2500
%	REAL ANO(4),AH(4),DNO(2),ST(3)
%	COMMON/ARGEXP/ARGMAX
  persistent MIRREQ;
  if isempty(MIRREQ)
	  MIRREQ = [ ...
                  1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, ...
                  1,-1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1, 1,-1, 1,-1, 1, ...
                 -1, 1,-1, 1,-1, 1, 1,-1, 1, 1,-1, 1,-1, 1, 1];
%///////////////////////////////////////////////////////////////////////////////
  end
  ANO = zeros(4,1);
  ST = zeros(3,1);
%     coefficients for mirroring
  for I=1:49
    D(1,3,I)=D(1,2,I)*MIRREQ(I);
    D(2,3,I)=D(2,2,I)*MIRREQ(I);
    D(3,3,I)=D(3,2,I)*MIRREQ(I);
    D(4,3,I)=D(4,2,I)*MIRREQ(I);
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
  if (DDD  >=  79)  &&  (DDD  <  171)
    SEZA=1;
    SEZB=2;
    DDDA=79;
    DDDB=171;
    DDDD=floor(double(DDD));
  end
%     21.6. - 22.9.
  if (DDD  >=  171)  &&  (DDD  <  265)
    SEZA=2;
    SEZB=4;
    DDDA=171;
    DDDB=265;
    DDDD=floor(double(DDD));
  end
%     23.9. - 20.12.
  if (DDD  >=  265)  &&  (DDD  <  354)
    SEZA=4;
    SEZB=3;
    DDDA=265;
    DDDB=354;
    DDDD=floor(double(DDD));
  end
%     21.12. - 20.3.
  if (DDD  >=  354)  ||  (DDD  <  79)
    SEZA=3;
    SEZB=1;
    DDDA=354;
    DDDB=365+79;
    %DDDD=DDD;
    if DDD  >=  354
      DDDD=floor(double(DDD));
    else
      DDDD=floor(double(DDD))+365;
    end
  end
  SEZAI=mod(SEZA-1,3)+1;
  SEZBI=mod(SEZB-1,3)+1;
%     550km level
  N0A550=0.0;
  N0B550=0.0;
  for I=1:49
    N0A550=N0A550+C(I)*D(1,SEZAI,I);
    N0B550=N0B550+C(I)*D(1,SEZBI,I);
  end
  N550A=N0A550;
  N550B=N0B550;
  N550=(N550B-N550A)/(DDDB-DDDA)*(DDDD-DDDA)+N550A;
%     900km level
  N0A900=0.0;
	N0B900=0.0;
	for I=1:49
    N0A900=N0A900+C(I)*D(2,SEZAI,I);
    N0B900=N0B900+C(I)*D(2,SEZBI,I);
  end
	N900A=N0A900;
	N900B=N0B900;
	N900=(N900B-N900A)/(DDDB-DDDA)*(DDDD-DDDA)+N900A;
%     1500km level
  N0A150=0.0;
  N0B150=0.0;
	for I=1:49
    N0A150=N0A150+C(I)*D(3,SEZAI,I);
    N0B150=N0B150+C(I)*D(3,SEZBI,I);
  end
	N150A=N0A150;
	N150B=N0B150;
	N1500=(N150B-N150A)/(DDDB-DDDA)*(DDDD-DDDA)+N150A;
%     2500km level
  N0A250=0.0;
	N0B250=0.0;
  for I=1:49
    N0A250=N0A250+C(I)*D(4,SEZAI,I);
    N0B250=N0B250+C(I)*D(4,SEZBI,I);
  end
  N250A=N0A250;
  N250B=N0B250;
  N2500=(N250B-N250A)/(DDDB-DDDA)*(DDDD-DDDA)+N250A;

%      IF (ALT  <  900) NO=(N900-N550)/350.0*(ALT-550)+N550
%      IF ((ALT  >=  900)  &&  (ALT  <  1500))
%     &  NO=(N1500-N900)/600.0*(ALT-900)+N900
%      IF (ALT  >=  1500) NO=(N2500-N1500)/1000.0*(ALT-1500)+N1500

%     O+ AND N+ may not increase above 1500km 
  if ((ION  ==  IRI2012.O_ION)  ||  (ION  ==  IRI2012.N_ION))  &&  (N2500  >  N1500)
   N2500=N1500;
  end
%     H+ may not decrease above 1500km 
  if (ION  ==  IRI2012.H_ION)  &&  (N2500  <  N1500)
    N2500=N1500;
  end
                   
  ANO(1)=N550;
	ANO(2)=N900;
  ANO(3)=N1500;
  ANO(4)=N2500;

	AH(1)=550.;
  AH(2)=900.;
  AH(3)=1500.;
  AH(4)=2250.;
  DNO(1)=20.;
  DNO(2)=20.;

  ST1=(ANO(2)-ANO(1))/(AH(2)-AH(1));
  for I=2:3
    ST2=(ANO(I+1)-ANO(I))/(AH(I+1)-AH(I));
    ANO(I)=ANO(I)-(ST2-ST1)*DNO(I-1)*log(2.);
    ST1=ST2;
  end

  for I=1:3
    ST(I)=(ANO(I+1)-ANO(I))/(AH(I+1)-AH(I));
  end

  %context.ARGEXP.ARGMAX=88.0;
  SUM=ANO(1)+ST(1)*(ALT-AH(1));

  for I=1:2
    AA = IRI2012.EPTR(ALT  ,DNO(I),AH(I+1));
    BB = IRI2012.EPTR(AH(1),DNO(I),AH(I+1));
    SUM=SUM+(ST(I+1)-ST(I))*(AA-BB)*DNO(I);
  end

  NION=10^SUM;

end

