% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if length(d_data)==2*10063 | length(d_data)==2*10567
  calTemp=[163 163 228 228];
  if all(ch_gain==ch_gain(1))
    if isempty(a_code)
      a_satch.clutter=repmat(a_satch.clutter,1,2);
      a_satch.repair=repmat(a_satch.repair,1,2);
    end
    ch_gain(3:4)=ch_gain(3:4)*0.78; % for 4 march 2007
  end
end
ng=length(ch_gain)/2;
glp=1886;
grps=[1 1 lpg_h(1);2 1884 lpg_h(1)+lpg_w(1)/2
      1885 1886 lpg_h(1886)];
for i=1:ng
  %aincorrect(glp,grps)
  glp=glp+1886; grps(:,1:2)=grps(:,1:2)+1886;
end
if exist('spear_count','var')
  spear_count=spear_count+(d_parbl(67)>1)
end

% 32m erroneously used instead of 42m for some time 2021-2022
if (d_date>=datenum(2021,12,13,00,00,00) & d_date<=datenum(2022,01,27,15,23,00))
    ch_gain=10^4.25;
    calTemp=228;
    ch_el=90;  % 32m parked at 90 degrees elevation during this period
    name_ant='32m';
    Magic_const=1.0;
end