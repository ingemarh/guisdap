function [ CLAR,CLOR,RBM,SLAC,SLOC ] = GEOLOW( context,SLAR,SLOR,RH )
%GEOLOW Calculates CGM coordinates from geocentric ones at low latitudes
%     SUBROUTINE GEOLOW(SLAR,SLOR,RH,CLAR,CLOR,RBM,SLAC,SLOC)
%  *********************************************************************
%  Calculates CGM coordinates from geocentric ones at low latitudes
%  where the DGRF/IGRF magnetic field lines may never cross the dipole
%  equatorial plane and, therefore, the definition of CGM coordinates
%  becomes invalid.
%
%  The code is written by Natalia and Vladimir Papitashvili as a part
%  of the earlier versions of GEO-CGM.FOR; extracted as a subroutine by
%  V. Papitashvili in February 1999.
%
%  Apr 11, 2001  GEOLOW is modified to account for interpolation of
%                CGM meridians near equator across the 360/0 boundary
%
%  See the paper by  Gustafsson, G., N. E. Papitashvili, and V. O.
%  Papitashvili, A revised corrected geomagnetic coordinate system for
%  Epochs 1985 and 1990 [J. Atmos. Terr. Phys., 54, 1609-1631, 1992]
%  for detailed description of the B-min approach utilized here.
%  *********************************************************************

%      COMMON /NM/NM
%      COMMON /IYR/IYR
%
%      DIMENSION BC(2),ARLAT(181),ARLON(181)
%      REAL*8 BM,B2,B3
  persistent NI NB;
  if isempty(NI)
    NI = 1+180;
    NB = 60;
  end
  ARLON = zeros(NI,1);
  ARLAT = zeros(NI,1);
  BC = zeros(2,1);
  %  This takes care if SLA is a dummy value (e.g., 999.99)

  if(SLAR == IGRF.BAD_ANGLE)
    CLAR = IGRF.BAD_ANGLE;
    CLOR = IGRF.BAD_ANGLE;
    SLAC = IGRF.BAD_ANGLE;
    SLOC = IGRF.BAD_ANGLE;
    RBM = IGRF.BAD_ANGLE;
    return;
  end

  %  HH is an error (nT) to determine B-min along the magnetic field line

  DHH = 0.5;

  %  Filling the work arrays of CGM latitudes and longitudes with 999.99
  %  Note that at certain geocentric longitudes in the very near-equator
  %  region no "geomagnetic equator" can be defined at all.

  for J=(1+NB):(NI-NB)
    ARLAT(J) = IGRF.BAD_ANGLE;
    ARLON(J) = IGRF.BAD_ANGLE;
  end

  SLO = SLOR;

  NDIR=0;

  %  Finding the geomagnetic equator as a projection of the B-min point
  %  found for the field lines started from the last latitude in each
  %  hemisphere where the CGM coordinates were obtained from geocentric
  %  ones (GEO --> CGM). First the CGM coordinates are calculated in the
  %  Northern (NDIR=0) and then in the Southern hemispheres (NDIR=1)

  %53
  while (NDIR == 0)

    %  Program works from 30 deg. latitude down to the geographic equator
    %  in the Northern Hemisphere

    for JC = (1+NB):(1+90)
      SLA = 90.-(JC-1);
      [~,~,CLA,CLO,~] = context.GEOCOR(SLA,SLO,RH);
      if(CLA == IGRF.BAD_ANGLE)
        break;
      end
      ARLAT(JC) = CLA;
      ARLON(JC) = CLO;
    end
    NDIR=1;
  end

  %  Program works from -30 deg. latitude down to the geographic equator
  %  in the Southern Hemisphere

  for JC = (NI-NB):-1:(2+90)
    SLA = 90.-(JC-1);
    [~,~,CLA,CLO,~] = context.GEOCOR(SLA,SLO,RH);
    if(CLA == IGRF.BAD_ANGLE)
      break;
    end
    ARLAT(JC) = CLA;
    ARLON(JC) = CLO;
  end
  %NDIR=0;
  %57

%  Finding last geographic latitudes along SLO where CGM coordinates
%  can be calculated

  n999=0;
  NDIR=0;
  for JC = (1+NB):(NI-NB)
    if(ARLAT(JC) == IGRF.BAD_ANGLE)
      if(NDIR == 0)
        jcn = JC - 1;
        rnlat = ARLAT(jcn);
        rnlon = ARLON(jcn);
        NDIR = 1;
        n999 = 1;
      end
    else %if(ARLAT(JC) ~= IGRF.BAD_ANGLE)
      if(NDIR == 1)
        jcs = JC;
        rslat = ARLAT(JC);
        rslon = ARLON(JC);
        %NDIR = 0;
        break;
      end
    end
  end
  %59

  %  If there is no points with 999.99 found along the SLO meridian,
  %  then the IHEM loop will start from 3; otherwise it starts from 1

  if(n999 == 0)
    ih = 3;
  else
    ih = 1;

%  Interpolation of the appropriate CGM longitudes between last
%  geocentric latitudes along SLO where CGM coordinates were defined
% (modified by Freddy Christiansen of DMI to account for interpolation
%  across the 360/0 boundary - April 11, 2001)

    rdel = jcs - jcn;
    if(rdel == 0.)
      delon = 0.;
    else
      if(rslon > 270. && rnlon < 90.)
        delon = (rslon - (rnlon + 360.))/rdel;
      else
        if(rslon < 90. && rnlon > 270.)
          delon = (rslon - (rnlon - 360.))/rdel;
        else
          delon = (rslon - rnlon)/rdel;
        end
      end
    end
    for JC = jcn+1:jcs-1
      ARLON(JC) = rnlon + delon*(JC-jcn);
      if (ARLON(JC) < 0.)
        ARLON(JC) = ARLON(JC) + 360.;
      end
    end
  end
  %31

  %  Finding the CGM equator at SLO on the sphere with radius RH

  NOBM = 0;
  for ihem = ih:3
    RM = RH;

    %  Defining the real equator point from the Northern Hemisphere

    if(ihem == 1)
      CLA = rnlat;
      SLA = 90. - (jcn - 1.);
      SLAN = SLA;
    end

    %  Defining the real equator point from the Southern Hemisphere

    if(ihem == 2)
      CLA = rslat;
      SLA = 90. - (jcs - 1);
      SLAS = SLA;
    end

    %  Defining the apex of the current magnetic field line

    if(ihem == 3)
      CLA = 0.;
      SLA = SLAR;
    end

    %  Here CLA is used only to calculate FRAC

    COL = (90. - CLA)*IGRF.UMR;
    SLM = (90. - SLA)*IGRF.UMR;
    SLL = SLO*IGRF.UMR;
    [BR,BT,BF] = IGRF.IGRFM(context.IYR,context.NM,RM,SLM,SLL);
    SZ = -BR;
    [XGEO,YGEO,ZGEO] = IGRF.SPHCAR(RM,SLM,SLL,IGRF.SPHERICAL_INPUT);
    BM = sqrt(BR*BR + BT*BT + BF*BF);
    XBM = XGEO;
    YBM = YGEO;
    ZBM = ZGEO;

    RL = 1./(sin(COL))^2;
    FRAC = 0.03/(1. + 3./(RL - 0.6));
    if(SZ <= 0.)
      FRAC = -FRAC;
    end
    DSD = RL*FRAC;
    DS = DSD;

    %5
    while true

      %  Keep two consequently computed points to define B-min

      for I = 1:2
        DD = DS;
        [XGEO,YGEO,ZGEO] = context.SHAG(XGEO,YGEO,ZGEO,DD);
        %11
        if(I == 1)
          XBM1 = XGEO;
          YBM1 = YGEO;
          ZBM1 = ZGEO;
          %RBM1 = sqrt(XBM1^2 + YBM1^2 + ZBM1^2);
        end
        %9

        [RM,SLM,SLL] = IGRF.SPHCAR(XGEO,YGEO,ZGEO,IGRF.CARTESIAN_INPUT);
        [BR,BT,BF] = IGRF.IGRFM(context.IYR,context.NM,RM,SLM,SLL);

        %  Go and compute the conjugate point if no B-min was found at this
        %  magnetic field line (could happen at very near geomagnetic equator)

        if(RM < RH)
          NOBM = 1;
          break;
        end

        BC(I) = sqrt(BR*BR + BT*BT + BF*BF);
      end
    %7
      if NOBM == 1
        break;
      end

      B2 = BC(1);
      B3 = BC(2);
      if(BM > B2 && B2 < B3)
        BB3 = abs(B3 - B2);
        BB2 = abs(BM - B2);
        if(BB2 < DHH && BB3 < DHH)
          break;
        end
        %BM = BM;
        XGEO = XBM;
        YGEO = YBM;
        ZGEO = ZBM;
        DS = DS/2.;
      elseif(BM >= B2 && B2 < B3) || ...
            (BM > B2 && B2 <= B3)
        %BM = BM;
        XGEO = XBM;
        YGEO = YBM;
        ZGEO = ZBM;
        DS = DS/2.;
      else
        BM = BC(1);
        XGEO = XBM1;
        YGEO = YBM1;
        ZGEO = ZBM1;
        XBM = XBM1;
        YBM = YBM1;
        ZBM = ZBM1;
      end
    end
    if NOBM == 1
      break;
    end

    [RBM1,RLA,~] = IGRF.SPHCAR(XBM1,YBM1,ZBM1,IGRF.CARTESIAN_INPUT);
    RLA = 90. - RLA/IGRF.UMR;
    %RLO = RLO/IGRF.UMR;

    if(ihem == 1)
      rlan = RLA;
    end
    if(ihem == 2)
      rlas = RLA;
    end

    %  Computation of the magnetically conjugate point at low latitudes

   %54
    if(ihem == 3)
      RBM = RBM1;
      %RM = RBM;
      DS = DSD;
   %55
      RR = RH + 1;
      while RR > RH
        [XBM1,YBM1,ZBM1] = context.SHAG(XBM1,YBM1,ZBM1,DS);
        RR = sqrt(XBM1^2 + YBM1^2 + ZBM1^2);
        if (RR > RH)
          R1 = RR;
          X1 = XBM1;
          Y1 = YBM1;
          Z1 = ZBM1;
        end
      end
      DR1 = abs(RH - R1);
      DR0 = abs(RH - RR);
      DR10 = DR1 + DR0;
      if(DR10 ~= 0.)
        DS = DS*(DR1/DR10);
        %RM = R1;
        [X1,Y1,Z1] = context.SHAG(X1,Y1,Z1,DS);
      end

      [~,SLAC,SLOC] = IGRF.SPHCAR(X1,Y1,Z1,IGRF.CARTESIAN_INPUT);
      SLAC = 90. - SLAC/IGRF.UMR;
      SLOC = SLOC/IGRF.UMR;
    end

%  End of loop IHEM
   %77
  end

  if (n999 ~= 0)

    if (NOBM == 1)

%  Interpolation of CGM latitudes if there is no B-min at this
%  magnetic field line

      rdel = jcs - jcn;
      if(rdel == 0.)
        delat = 0.;
      else
        delat = (rslat - rnlat)/rdel;
      end
      jdel = 0;
      for jc=jcn+1:jcs-1
        jdel = jdel + 1;
        ARLAT(jc) = rnlat + delat*jdel;
      end
      RBM = IGRF.BAD_ANGLE;
      SLAC = IGRF.BAD_ANGLE;
      SLOC = IGRF.BAD_ANGLE;

    else

%  Geocentric latitude of the CGM equator

	    RLA = (rlan + rlas)/2.;

%  Interpolation of the CGM latitudes in the Northern hemisphere

	    rdel = SLAN - RLA;
      if(rdel == 0.)
        delat = 0.;
      else
        delat = rnlat/rdel;
      end
      jdn = abs(rdel);
      jdel = 0;
      for jc = jcn+1:jcn+jdn
        jdel = jdel + 1;
        ARLAT(jc) = rnlat - delat*jdel;
      end

%  Interpolation of the CGM latitudes in the Southern hemisphere

	    rdel = SLAS - RLA;
      if(rdel == 0.)
        delat = 0.;
      else
        delat = rslat/rdel;
      end
      %jds = abs(rdel);
      jdel = 0;
      for jc = jcs-1:-1:jcs-jd
        jdel = jdel + 1;
        ARLAT(jc) = rslat + delat*jdel;
      end
    end

  end

%  Defining by interpolation the exact values of the CGM latitude
%  and longitude between two adjacent values

  L1 = 90. - SLAR + 1.;
  if(SLAR < 0.)
    L2 = L1-1;
  else
    L2 = L1+1;
  end
  DSLA =  abs(SLAR - floor(SLAR));
  DELCLA = ARLAT(L2) - ARLAT(L1);
  DELCLO = ARLON(L2) - ARLON(L1);
  CLAR = ARLAT(L1) + DELCLA*DSLA;
  CLOR = ARLON(L1) + DELCLO*DSLA;

end

