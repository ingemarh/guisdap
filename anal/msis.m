function [profile,status]=msis(height,time,position,indexes,cgs)

% MSIS Neutral Atmosphere Empirical Model 
% $Id$
%
%  [profile,status]=msis(height,time,position,indexes,cgs)
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
error('There should be a mex-file with this name, this is just a help message')
