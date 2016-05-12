% guizard.m: special experiment specific hacks
% GUISDAP v8.5   08-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
lpgs=[2 3 564 565]; lpg_bac(lpgs)=lpg_bac(lpgs)+1;
if d_date<datenum(2015,12,15) % Bug on the topside data adressing
 lpgs=1125:1435; lpg_bcs(lpgs)='g';
end
