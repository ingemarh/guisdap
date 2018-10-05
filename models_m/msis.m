function [profile,status]=msis(height,time,position,indices,cgs)

% MSIS Neutral Atmosphere Empirical Model 
% $Id$
%
%  [profile,status]=msis(height,time,position,indices,cgs)
%
%   Neutral Atmosphere Empirical Model from the surface to lower
%   exosphere  MSISE90 (JGR, 96, 1159-1172, 1991)
%   A.E.Hedin 4/24/90;6/3/91(add SAVE)
%   2/11/93 correct switch initialization and mks calculation
%   Matlab interface 1998-01-14 by Mikael Hedin <micce@irf.se>
%
%  INPUT:
%     height - scalar or vector (max length 1024) of altitude [m]
%     time - [day_of_year UT_in_sec]
%     position - [lat long] (decimal degrees)
%    optional:
%     indexes - [f107a f107 Ap] (default [100 100 5])
%     cgs -  use cgs units if nonzero value, else SI
%
%  OUTPUT:
%    profile - Xx9-matrix (with one row for each height):
%      [He, O, N2, O2, Ar, H, N] number densities
%      Total (mass density), 
%      Temperature at altitude
%    status - 2-vector:
%      exospheric temperature (K)
%      cgs-flag (cgs-units if 1, SI-units if 0)
%
%    The units are SI ([m-3] and [kg/m3]) or CGS ([cm-3] 
%    and [g/cm3]) depending on cgs-flag
if nargin<4, indices=[]; end
if nargin<5, cgs=[]; end
if isempty(indices), indices=[100 100 5]; end
if isempty(cgs), cgs=0; end

xlat=position(1); xlon=position(2);
iyd=time(1); sec=time(2);
stl=sec/3600+xlon/15;
hxx=height/1000.;
f107a=indices(1); f107=indices(2); ap=indices(3);

%ap = zeros(CIRA.maxAP,1); % magnetic index
ap(CIRA.DAILY_AP) = ap;
%ap(CIRA.CURRENT_AP) = 4.2;
%ap(CIRA.CURRENT_M_3_AP) = 4.3;
%ap(CIRA.CURRENT_M_6_AP) = 4.4;
%ap(CIRA.CURRENT_M_9_AP) = 4.5;
%ap(CIRA.CURRENT_M_12_33_AP) = 4.6;
%ap(CIRA.CURRENT_M_36_57_AP) = 4.7;
atm = CIRA();
mass = atm.MT(CIRA.ALL_MASS); % calculate all constituents
sw = CIRA.allSwitchesOn;
sw(CIRA.TURBO_SCALE_SW) = 0; % turn off turbo scale option
atm = atm.TSELEC(sw);
atm.METERS(1-cgs);
profile=NaN*ones(length(hxx),9); i=0;
for h=row(hxx)
 i=i+1;
 [ D,T,atm ] = atm.GTD7(iyd,sec,h,xlat,xlon,stl,f107a,f107,ap,mass);
 profile(i,:)=[D([1 2 3 4 5 7 8 6])' T(2)];
end
status=[T(1);atm.IMR];
