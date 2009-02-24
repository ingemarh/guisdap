% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if d_date<datenum(2002,6,1) & lpg_ra(478)==15537
% noise injection moved
  lpgs=find(lpg_lag>0);
  lpg_ra(lpgs)=lpg_ra(lpgs)+12;
% data dump changes with integration time
  shift=length(d_data)/2-15537;
  lpg_ra(478:end)=lpg_ra(478:end)+shift;
  form_adpar
end


glp=[476 953];
grps=[1 1 lpg_h(1);2 475 lpg_h(1)+lpg_w(1)/2
      476 477 lpg_h(476)
      478 478 lpg_h(478);479 953 lpg_h(478)+lpg_w(478)/2];
gaincorrect(glp,grps)
