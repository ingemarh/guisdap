% lpg_lp.m: Gives the lag profiles that belong to a lag profile group
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function lp=lpg_lp(lpg)

function lp=lpg_lp(lpg)

global lpg_lpdata lpg_lpstart lpg_lpend
lp=lpg_lpdata(lpg_lpstart(lpg):lpg_lpend(lpg));
