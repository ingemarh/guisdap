% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='L'
% lpg_bcs([1 3 4])='x';
elseif name_site=='V'
 if d_date>=datenum(2001,11,05,13,30,0) & d_date<=datenum(2001,11,05,22,27,0)
   ch_el=45;
 end
end
