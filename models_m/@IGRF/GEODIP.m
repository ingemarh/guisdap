function [ lat, lon, context ] = GEODIP( context, IYR,latin,lonin,J )
%GEODIP Calculates dipole geomagnetic coordinates from geocentric coordinates
% or vice versa
%      SUBROUTINE GEODIP(IYR,SLA,SLO,DLA,DLO,J)
%
%  Calculates dipole geomagnetic coordinates from geocentric coordinates
%  or vice versa.
%
%                     J=0           J=1
%		INPUT:     J,SLA,SLO     J,DLA,DLO
%		OUTPUT:     DLA,DLO       SLA,SLO
%
%  Last revision: November 2005 (Vladimir Papitashvili)
%  The code is modifed from GEOCOR written by V.Popov and V.Papitashvili
%  in mid-1980s. 
%
%         COMMON /CONST/UMR 
%
%  Earth's radius (km) RE = 6371.2
%
%  The radius of the sphere to compute the coordinates (in Re)
%        RH = (RE + HI)/RE
  R = 1.;
  context = context.RECALC(IYR,0,25,0,0);

  if J == IGRF.GEOGRAPHIC_COORDINATES
    SLA = latin;
    SLO = lonin;
    COL = (90.- SLA)*IGRF.UMR;
    RLO = SLO*IGRF.UMR;
    [X,Y,Z] = IGRF.SPHCAR(R,COL,RLO,IGRF.SPHERICAL_INPUT);
    [XM,YM,ZM] = context.GEOMAG(X,Y,Z,IGRF.GEOGRAPHIC_COORDINATES);
    [~,TH,PF] = IGRF.SPHCAR(XM,YM,ZM,IGRF.CARTESIAN_INPUT);
    %SZM = ZM;
    DLO = PF/IGRF.UMR;
    DCO = TH/IGRF.UMR;
    DLA = 90.- DCO;
    lat = DLA;
    lon = DLO;
  else          
    DLA = latin;
    DLO = lonin;
    COL = (90.- DLA)*IGRF.UMR;
    RLO = DLO*IGRF.UMR;
    [XM,YM,ZM] = IGRF.SPHCAR(R,COL,RLO,IGRF.SPHERICAL_INPUT);
    [X,Y,Z] = context.GEOMAG(XM,YM,ZM,IGRF.GEOMAGNETIC_COORDINATES);
    [~,TH,PF] = IGRF.SPHCAR(X,Y,Z,IGRF.CARTESIAN_INPUT);
    %SZM = ZM;
    SLO = PF/IGRF.UMR;
    SCO = TH/IGRF.UMR;
    SLA = 90.- SCO;
    lat = SLA;
    lon = SLO;
  end

end

