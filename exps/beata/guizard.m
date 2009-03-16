% guizard.m: special experiment specific hacks
% GUISDAP v8.5   06-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if name_site=='T'
 lpgs=find(ismember(lpg_code,3:16));
 lpg_wom(lpgs,:)=lpg_wom(lpgs,:)/14;
 lpg_w(lpgs)=lpg_w(lpgs)/14;
 lpg_wr(:,lpgs)=lpg_wr(:,lpgs)/14;
 if d_date<datenum(2007,1,1)
  lpg_ra(1739:1740)=[25547 25563]; a_code=1:2;
 elseif isempty(a_code)
  if a_pponly
   a_code=2:16;
  else
   a_code=1:2;
  end
 end
 if ~isempty(a_code) | min(a_code)<1
  lpg_code(1:2)=a_code(1);
 end
elseif name_site=='L'
 lpgs=find(ismember(lpg_code,3:17));
 lpg_wom(lpgs,:)=lpg_wom(lpgs,:)/15;
 lpg_w(lpgs)=lpg_w(lpgs)/15;
 lpg_wr(:,lpgs)=lpg_wr(:,lpgs)/15;
 if d_date<datenum(2007,1,1)
  lpg_ra(1618:1619)=lpg_ra(1618:1619)-161*15; a_code=1:2;
 elseif isempty(a_code)
  if a_pponly
   a_code=2:17;
  else
   a_code=1:2;
  end
 end
 if ~isempty(a_code) | min(a_code)<1
  lpg_code(1:2)=a_code(1);
 end
 if length(d_data)>11370
  lpg_rep(2)
  if ~exist('analysis_code')
   a_code=[a_code a_code+17];
  end
 end
end
