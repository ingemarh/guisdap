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
if lpb>57 & d_parbl(58)>0
  d_rcprog=d_parbl(58);
else
  d_rcprog=a_rcprog;
end
 
if name_site=='K'
  p_RECloc=[67.863, 20.44, .412];
elseif name_site=='S'
  p_RECloc=[67.367, 26.65, .180];
end

% endtime of integration 
[time,year]=tosecs(d_parbl(1:6)); 
d_time(2,:)=toYMDHMS(year,time);
% starttime of integration 
time=time-d_parbl(7);
d_time(1,:)=toYMDHMS(year,time);

ch_Pt=d_parbl(a_txpower(1))*a_txpower(2);
ch_el=d_parbl(9);
ch_az=d_parbl(10);
if name_site=='V'
 ch_el=d_parbl(9:10);
end
if lpb>41 & (name_site=='K' | name_site=='S')
  ch_range=d_parbl(42)/1000;
else
  ch_range=[];
end
calTemp=d_parbl(21);
