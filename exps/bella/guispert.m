% guispert.m: special experiment specific hacks
% GUISDAP v8.5   06-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if name_site=='V'
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
end
