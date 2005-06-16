% spektri_init.m: loads the plasma dispersion function table pldfvv
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: spec
%
% function spektri_init
function spektri_init
global pldfvv path_GUP
% Contains the plasma dispersion function values for interpolation
load(canon(fullfile(path_GUP,'matfiles','pldfvv')));
