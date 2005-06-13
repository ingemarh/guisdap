% guispert.m: special experiment specific hacks
% GUISDAP v8.4   05-06-10 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if strcmp(name_ant(1:3),'vhf')
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
end
