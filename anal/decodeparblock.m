% decodeparblock
% GUISDAP v1.81   03-01-27 Copyright EISCAT, Huuskonen&Lehtinen
% simple parameter block decoding the old eros3/esr system
% it shows what parameters must be obtained with the data
% so that the data analysis is possible

% names='LLVTKS'; name_site=names(parbl(41)); clear names
lpb=length(d_parbl);
if lpb<41
  ant_id=1;
else
  ant_id=d_parbl(41);
  if ant_id==0
    ant_id=3; % For the May2000 data
  end
end
if exist('f_d_rcprog','var')
  d_rcprog=f_d_rcprog;
%elseif lpb>56 & d_parbl(57)>0
%  d_rcprog=d_parbl(57);
else
  d_rcprog=a_rcprog;
end
 
if ant_id==5
  p_RECloc=[67.863, 20.44, .412];
elseif ant_id==6
  p_RECloc=[67.367, 26.65, .180];
end

% endtime of integration 
d_time(2,:)=row(d_parbl(1:6)); 
% starttime of integration 
d_time(1,:)=timeconv(timeconv(d_time(2,:),'utc2tai')-d_parbl(7),'tai2utc');

ch_Pt=d_parbl(a_txpower(1))*a_txpower(2);
ch_el=d_parbl(9);
ch_az=d_parbl(10);
if ant_id==3
  ch_el=d_parbl(9:10);
  ch_az=zeros(size(ch_el));
end
if lpb>41 & (ant_id==5 | ant_id==6) & d_parbl(42)
  ch_range=d_parbl(42)/1000;
else
  ch_range=[];
end
calTemp=d_parbl(21);
