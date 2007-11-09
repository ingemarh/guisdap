% guizard.m: special experiment specific hacks
% GUISDAP v8.5   07-10-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
nsh=max(lpg_ra+(lpg_nt-1).*lpg_ri)+1;
if name_site=='V'
 lpg_rep(2,nsh)
elseif name_site=='K'
 lpg_cal(1:end)=find(lpg_bcs=='b' & lpg_lag==0);
 if any(a_code>2)
  lpg_rep(4)
 else
  lpg_rep(2,nsh)
 end
end
