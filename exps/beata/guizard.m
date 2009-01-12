% guizard.m: special experiment specific hacks
% GUISDAP v8.5   06-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
d_date=datenum(d_time(1,:));
if name_site=='T'
 lpgs=find(ismember(lpg_code,3:16));
 lpg_wom(lpgs,:)=lpg_wom(lpgs,:)/14;
 lpg_w(lpgs)=lpg_w(lpgs)/14;
 lpg_wr(:,lpgs)=lpg_wr(:,lpgs)/14;
 if d_date<2007
  lpg_ra(1739:1740)=[25547 25563]; a_code=1:2;
 elseif a_pponly & isempty(a_code)
  a_code=2:16;
 elseif ~a_pponly
  a_code=1:2;
 end
 if ~isempty(a_code) | min(a_code)<1
  lpg_code(1:2)=a_code(1);
 end
end
