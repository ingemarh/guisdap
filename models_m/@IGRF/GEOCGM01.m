function [ DAT,PLA,PLO, context ] = GEOCGM01( context, ICOR,IYEAR,HI,DAT )
%GEOCGM01 GEOCGM01
%      SUBROUTINE GEOCGM01(ICOR,IYEAR,HI,DAT,PLA,PLO)
%  *********************************************************************
%  Version 2011 for GEO-CGM.FOR    (good through 2015)      January 2011
%  Version 2005 for GEO-CGM.FOR    (good through 2010)     November 2005
%  Nov 11, 2005  IGRF and RECALC are is modified to the IGRF-10 model 
%                and extended back to 1900 using the DGRF coeffcients
%  Apr 11, 2001  GEOLOW is modified to account for interpolation of
%                CGM meridians near equator across the 360/0 boundary
%  AUTHORS:
%  Natalia E. Papitashvili (WDC-B2, Moscow, Russia, now at NSSDC,
%    NASA/Goddard Space Flight Center, Greenbelt, Maryland)
%  Vladimir O. Papitashvili (IZMIRAN, Moscow, Russia, now at SPRL,
%    University of Michigan, Ann Arbor)
%  Conributions from Boris A. Belov and Vladimir A. Popov (both at
%    IZMIRAN), Therese Moretto (DMI, DSRI, now at NSF), Freddy 
%    Christiansen (DMI, DSRI), and Scott Boardsen (NASA/GSFC).
%
%  The original version of this code is described in the brochure by
%  N.A. Tsyganenko, A.V. Usmanov, V.O. Papitashvili, N.E. Papitashvili,
%  and V.A. Popov, Software for computations of geomagnetic field and
%  related coordinate systems, Soviet Geophys. Committ., Moscow, 58 pp.,
%  1987. A number of subroutines from the revised GEOPACK-96 software
%  package developed by Nikolai A. Tsyganenko and Mauricio Peredo are
%  utilized in this code with some modifications (see full versions of
%  GEOPACK packages on http://www-spof.gsfc.nasa.gov/Modeling/geopack.html).
%
%  This code consists of the main subroutine GEOCGM01, five functions
%  (OVL_ANG, CGMGLA, CGMGLO, DFRIDR, and AZM_ANG), eigth new and revised
%  subroutines from the above-mentioned brochure (MLTUT, MFC, FTPRNT,
%  GEOLOW, CORGEO, GEOCOR, SHAG, and RIGHT), and 9 subroutines from
%  GEOPACK-96 (IGRF, SPHCAR, BSPCAR, GEOMAG, MAGSM, SMGSM, RECALC, SUN)
%
%  =====================================================================
%
%  Input parameters:
%     ICOR = +1    geo to cgm
%            -1    cgm to geo
%     IYEAR= year
%     HI   = altitude in km
%  Input/Output parameters:
%     DAT(1,i)=slar geocentric latitude (input/output if icor=+1/-1)
%     DAT(2,i)=slor geocentric longitude (input/output if icor=+1/-1)
%     DAT(3,i)=clar CGM latitude (input/output if icor=-1/+1)
%     DAT(4,i)=clor CGM longitude (input/output if icor=-1/+1)
%  Output parameters:
%     DAT(5,i)=rbm apex of the magnetic field line in Re (Re=6371.2 km)
%            (this parameter approximately equals the McIlwain L-value)
%     DAT(6,i)=btr IGRF Magnetic field H (nT)
%     DAT(7,i)=brr IGRF Magnetic field D (deg)
%     DAT(8,i)=ovl oval_angle as the azimuth to "magnetic north":
%                + east in Northern Hemisphere
%                + west in Southern Hemisphere
%     DAT(9,i)=azm meridian_angle as the azimuth to the CGM pole:
%                + east in Northern Hemisphere
%                + west in Southern Hemisphere
%     DAT(10,i)=utm magnetic local time (MLT) midnight in UT hours
%     		 i=1	for the start point
%     		 i=2	for the conjugate point of the start point (slac, sloc)
%			 i=3    for the footprint at 1-Re of the start point (slaf,slof)
%			 i=4    for the conjugate footprint at 1-Re of the start point
%     PLA(1)	geocentric latitude of the CGM pole in the Northern hemisphere
%     PLO(1)	geocentric longitude of the CGM pole in the Northern hemisphere
%     PLA(2)	geocentric latitude of the CGM pole in the Southern hemisphere
%     PLO(2)	geocentric longitude of the CGM pole in the Southern hemisphere
%     PLA(3)	geoce lati CGM North pole at the Earth's surface 1-Re or zero alt.
%     PLO(3)	geoce long CGM North pole at the Earth's surface 1-Re or zero alt.
%     PLA(4)	geoce lati CGM South pole at the Earth's surface 1-Re or zero alt.
%     PLO(4)	geoce long CGM South pole at the Earth's surface 1-Re or zero alt.
%
% In program:
%     dla  = dipole latitude
%     dlo  = dipole longitude
%
%  =====================================================================

%      COMMON /C1/ AA(27),II(2),BB(8)
%      COMMON /IYR/ IYR
%      COMMON /NM/ NM
%      COMMON /RZ/ RH

%      DIMENSION DAT(11,4),PLA(4),PLO(4)
%      CHARACTER STR*12
  %NI = 11;
  NJ = 4;

  %  Year (for example, as for Epoch 1995.0 - no fraction of the year)
  PLA = zeros(4,1);
  PLO = zeros(4,1);

  context.IYR = double(IYEAR);
  context = context.RECALC(IYEAR,0,25,0,0);

  %  Earth's radius (km)

  RE = context.ERA;

  %  NM is the number of harmonics

  context.NM = 10;

  %  The radius of the sphere to compute the coordinates (in Re)

  RH = (RE + HI)/RE;

%  Correction of latitudes and longitudes if they are entered beyond of
%  the limits (this actually does not affect coordinate calculations
%  but the oval/meridian angles and MLT midnight cannot be computed)

  if DAT(1,1) > 90.
    DAT(1,1) =  180. - DAT(1,1);
  elseif DAT(1,1) < -90.
    DAT(1,1) = -180. - DAT(1,1);
  end
  if DAT(3,1) > 90.
    DAT(3,1) =  180. - DAT(3,1);
  elseif DAT(3,1) < -90.
    DAT(3,1) = -180. - DAT(3,1);
  end

  if DAT(2,1) >  360.
    DAT(2,1) = DAT(2,1) - 360.;
  elseif DAT(2,1) < -360.
    DAT(2,1) = DAT(2,1) + 360.;
  end
  if DAT(4,1) > 360.
    DAT(4,1) = DAT(4,1) - 360.;
  elseif DAT(4,1) < -360.
    DAT(4,1) = DAT(4,1) + 360.;
  end

%  Computation of CGM coordinates from geocentric ones at high- and
%  middle latitudes

  if ICOR == 1

    SLAR = DAT(1,1);
    SLOR = DAT(2,1);
    if abs(SLAR) == 90.
      SLOR = 360.;
    end
    [~,~,CLAR,CLOR,PMR] = context.GEOCOR(SLAR,SLOR,RH);
    DAT(3,1) = CLAR;
    DAT(4,1) = CLOR;

  else

%  Computation of geocentric coordinates from CGM ones at high- and
%  middle latitudes

    CLAR = DAT(3,1);
    CLOR = DAT(4,1);
    if abs(CLAR) == 90.
      CLOR = 360.;
    end
    [SLAR,SLOR,~,~,PMR] = context.CORGEO(RH,CLAR,CLOR);
    DAT(1,1) = SLAR;
    DAT(2,1) = SLOR;

  end

%  PMI is L-shell parameter for the magnetic field line; limit to 16 Re

  if PMR >= 16.
    PMR = IGRF.BAD_ANGLE;
  end
  DAT(5,1) = PMR;

%  Check if CGM_Lat has been calculated, then go for the conjugate point

  if CLAR == IGRF.BAD_ANGLE

%  CGM_Lat has NOT been calculated, call GEOLOW for computation of the
%  CGM coordinates at low latitudes using the CBM approach (see the
%  reference in GEOLOW)

    [CLAR,CLOR,RBM,SLAC,SLOC] = context.GEOLOW(SLAR,SLOR,RH);
    DAT(3,1) = CLAR;
    DAT(4,1) = CLOR;
    if RBM >= 16.
      RBM = IGRF.BAD_ANGLE;
    end
    DAT(5,1) = RBM;

%  Conjugate point coordinates at low latitudes

    sprintf(STR,'%6.2f',SLAC);
    SLAC = str2double(STR);
    sprintf(STR,'%6.2f',SLOC);
    SLOC = str2double(STR);
    DAT(1,2) = SLAC;
    DAT(2,2) = SLOC;
    [~,~,CLAC,CLOC,RBM] = context.GEOCOR(SLAC,SLOC,RH);
    if CLAC > 999.
      [CLAC,CLOC,RBM,~,~] = context.GEOLOW(SLAC,SLOC,RH);
    end
    DAT(3,2) = CLAC;
    DAT(4,2) = CLOC;
    DAT(5,2) = RBM;

  else

%  Computation of the magnetically conjugated point at high- and
%  middle latitudes

    CLAC = -CLAR;
    CLOC =  CLOR;
    DAT(3,2) = CLAC;
    DAT(4,2) = CLOC;
    [SLAC,SLOC,~,~,PMC] = context.CORGEO(RH,CLAC,CLOC);
    DAT(1,2) = SLAC;
    DAT(2,2) = SLOC;
    if PMC >= 16.
      PMC = IGRF.BAD_ANGLE;
    end
    DAT(5,2) = PMC;
  end

  %  Same RBM for footprints as for the starting and conjugate points

  DAT(5,3) = DAT(5,1);
  DAT(5,4) = DAT(5,2);

  %  Calculation of the magnetic field line footprint at the
  %  Earth's surface for the starting point

  if RH > 1. && CLAR ~= IGRF.BAD_ANGLE && CLOR ~= IGRF.BAD_ANGLE
    [ACLAR,ACLOR,SLARF,SLORF] = context.FTPRNT(RH,SLAR,SLOR,CLAR,CLOR,1.);
    DAT(1,3) = SLARF;
    DAT(2,3) = SLORF;
    DAT(3,3) = ACLAR;
    DAT(4,3) = ACLOR;
    %  and for the conjugate point
    [ACLAC,ACLOC,SLACF,SLOCF] = context.FTPRNT(RH,SLAC,SLOC,CLAC,CLOC,1.);
    DAT(1,4) = SLACF;
    DAT(2,4) = SLOCF;
    DAT(3,4) = ACLAC;
    DAT(4,4) = ACLOC;
  else
    for i = 1:4
      for j = 3:NJ
        DAT(i,j) = IGRF.BAD_ANGLE;
      end
    end
  end

  %  Computation of geocentric coordinates of the North or South CGM
  %  poles for a given year at the altitude RH and Earth's surface (1-Re)

  [PLAN,PLON,~,~,~] = context.CORGEO(RH, 90.,360.);
  PLAN1 = PLAN;
  PLON1 = PLON;

  [PLAS,PLOS,~,~,~] = context.CORGEO(RH,-90.,360.);
  PLAS1 = PLAS;
  PLOS1 = PLOS;

  if RH > 1.
    [PLAN1,PLON1,~,~,~] = context.CORGEO(1., 90.,360.);
    [PLAS1,PLOS1,~,~,~] = context.CORGEO(1.,-90.,360.);
  end

   if CLAR < 0.
     PLA(1) = PLAS;
     PLO(1) = PLOS;
   else
     PLA(1) = PLAN;
     PLO(1) = PLON;
   end
   if ACLAR < 0.
     PLA(3) = PLAS1;
     PLO(3) = PLOS1;
   else
     PLA(3) = PLAN1;
     PLO(3) = PLON1;
   end
   if CLAC < 0.
     PLA(2) = PLAS;
     PLO(2) = PLOS;
   else
     PLA(2) = PLAN;
     PLO(2) = PLON;
   end
   if ACLAC < 0.
     PLA(4) = PLAS1;
     PLO(4) = PLOS1;
   else
     PLA(4) = PLAN1;
     PLO(4) = PLON1;
   end

  for j = 1:NJ
    DAT( 6,j) = 99999.;
    DAT( 7,j) = IGRF.BAD_ANGLE;
    DAT( 8,j) = 99999.;
    DAT( 9,j) = IGRF.BAD_ANGLE;
    DAT(10,j) = IGRF.BAD_ANGLE;
    DAT(11,j) =  99.99;
  end

  icount = 2;
  if RH > 1.
    icount = NJ;
  end
  RJ = RH;
  for j = 1:icount
    if j > 2
      RJ = 1.;
    end

    PLAJ = PLA(j);
    PLOJ = PLO(j);

    SLAJ = DAT(1,j);
    SLOJ = DAT(2,j);
    CLAJ = DAT(3,j);
    CLOJ = DAT(4,j);

    %  Computation of the IGRF components
    [BTR,BFR,BRR] = context.MFC(SLAJ,SLOJ,RJ);
    DAT(6,j) = BTR;
    DAT(7,j) = BFR;
    DAT(8,j) = BRR;

    %  Computation of the oval_angle (OVL) between the tangents to
    %  geographic and CGM latitudes at a given point (the code is slightly
    %  modified from the source provided by Therese Morreto in 1994). Note
    %  that rotation of OVL on 90 deg anticlockwise provides the azimuth
    %  to the local "magnetic" north (south) measured from the local
    %  geographic meridian. The OVL_ANG can be calculated only at middle
    %  and high latitudes where CGM --> GEO is permitted.

    OVL = context.OVL_ANG(SLAJ,SLOJ,CLAJ,CLOJ,RJ);
    DAT(9,j) = OVL;

    %  Computation of the meridian_angle (AZM) between the geographic
    %  meridian and direction (azimuth along the great-circle arc) to
    %  the North (South) CGM pole

    AZM = IGRF.AZM_ANG(SLAJ,SLOJ,CLAJ,PLAJ,PLOJ);
    DAT(10,j) = AZM;

    %  Computation of the MLT midnight (in UT)
    UT = IGRF.MLTUT(SLAJ,SLOJ,CLAJ,PLAJ,PLOJ);
    DAT(11,j) = UT;

    %  End of loop j = 1,icount
  end

end

