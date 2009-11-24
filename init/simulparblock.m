% simulparblock: Radar control parameters for simulated data
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%function  simulparblock

d_rcprog=1;
d_time(1,:)=toYMDHMS(a_year,a_start);
d_time(2,:)=toYMDHMS(a_year,a_end);
len=length(ch_fradar);
ch_Pt=a_simul(3)*ones(1,len);
ch_range=a_simul(5)*ones(1,len);
ch_az=a_simul(6)*ones(1,len);
ch_el=a_simul(7)*ones(1,len);
ant_id=strfind('LLVTKS',name_site);
