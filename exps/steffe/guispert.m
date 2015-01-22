% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if exist('skip_upper_tail','var') & skip_upper_tail
  lpg_bcs(find(lpg_h>3200 & lpg_h<3800 & lpg_nt==2))='x';
end 
if d_date<datenum(2003,11,11,21,15,0)
  if ant_id==1
    name_ant='42m'; ch_el=81.6; ch_az=181; calTemp=163;
    ch_gain=10^4.52;
  elseif ant_id==2
    name_ant='32m'; calTemp=228;
    ch_gain=10^4.25;
    ddate=(d_date-datenum(2003,11,11,9,9,0))*86400;
    if ddate>0
      if ~exist('pos_cp2','var')
        global d_filelist
        d=rem(d_filelist-tosecs([2003 11 11 9 0 0]),3*128);
        pos_cp2=ceil(ceil([27.6 26.8 27.6]/6.4)*6.4)+[1 3 5]*64;
        d=find((d>64 & d<=pos_cp2(1)) | (d>64*3 & d<=pos_cp2(2)) | (d>64*5 & d<=pos_cp2(3)));
        d_filelist(d)=[];
        pos_cp2=[144 171.6 171.6;66.66 90 63.2];
      end
      d_pos=fix(rem(ddate/128,3))+1;
      ch_el=pos_cp2(2,d_pos); ch_az=pos_cp2(1,d_pos);
    end
  end
end
if length(d_data)>34863 & d_rcprog==2
  calTemp=[163 163 228 228];
  if all(ch_gain==ch_gain(1))
%   if isempty(a_code)
      a_satch.clutter=repmat(a_satch.clutter,1,2);
      a_satch.repair=repmat(a_satch.repair,1,2);
%   end
    ch_gain(3:4)=ch_gain(3:4)*0.64;
  end
end
if d_rcprog==2
  ng=length(ch_gain)/2;
  glp=1696;
  grps=[1 1 lpg_h(1);2 1695 lpg_h(1)+lpg_w(1)/2
        1696 1697 lpg_h(1696)
        1698 1698 lpg_h(1698);1699 2314 lpg_h(1698)+lpg_w(1698)/2
        2315 2315 lpg_h(2315);2316 2598 lpg_h(2315)+lpg_w(2315)/2];
  for i=1:ng
    gaincorrect(glp,grps)
    glp=glp+2598; grps(:,1:2)=grps(:,1:2)+2598;
  end
else
  glp=767;
  grps=[1 1 lpg_h(1);2 768 lpg_h(1)+lpg_w(1)/2
        767 768 lpg_h(767)
        769 769 lpg_h(769);770 1129 lpg_h(769)+lpg_w(769)/2
        1130 1130 lpg_h(1130);1131 1412 lpg_h(1130)+lpg_w(1130)/2];
  gaincorrect(glp,grps)
end
