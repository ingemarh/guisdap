% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
d_date=datenum(d_time(1,:));
if d_date<datenum(2003,11,11,21,15,0)
  if ant_id==1
    name_ant='42m'; ch_el=81.6; ch_az=181; calTemp=163;
    ch_gain=10^4.52;
  elseif ant_id==2
    name_ant='32m'; calTemp=228;
    ch_gain=10^4.25;
    d_date=(d_date-datenum(2003,11,11,9,9,0))*86400;
    if d_date>0
      if ~exist('pos_cp2','var')
        global d_filelist
        d=rem(d_filelist-tosecs([2003 11 11 9 0 0]),3*128);
        pos_cp2=ceil(ceil([27.6 26.8 27.6]/6.4)*6.4)+[1 3 5]*64;
        d=find((d>64 & d<=pos_cp2(1)) | (d>64*3 & d<=pos_cp2(2)) | (d>64*5 & d<=pos_cp2(3)));
        d_filelist(d)=[];
        pos_cp2=[144 171.6 171.6;66.66 90 63.2];
      end
      d_pos=fix(rem(d_date/128,3))+1;
      ch_el=pos_cp2(2,d_pos); ch_az=pos_cp2(1,d_pos);
    end
  end
end
