% guispert.m: special experiment specific hacks
% GUISDAP v8.4   05-06-10 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if strcmp(name_ant(1:3),'vhf')
  if (d_date>=datenum(2012,06,12,06,00,0) & d_date<=datenum(2012,07,12,23,00,0))| fix(d_date)==fix(datenum(2012,09,04,0,0,0)) | fix(d_date)==fix(datenum(2012,09,06,0,0,0)) | fix(d_date)==fix(datenum(2012,09,07,0,0,0))
    ch_Pt=d_parbl(75)*9.59*1e3;
    fprintf('GUISPERT: fixing vhf tx power estimate using average WG power/9.59 kW\n')
%  The peak power reading from the waveguide varied because of an intermittent 
%  bad connection. The problem was apparently fixed in the morning of 13 July 2012 (??)
%  The factor 9.59 was found from a regression between parbl(70)
%  = peak power and parbl(75)=average power from 14 June 2012 for an interval when
%  peak power was correct, ca. 14 UT.
 end
end

if strcmp(name_ant(1:3),'kir') & d_date>=datenum(2020,08,13,00,00,00) & d_date<datenum(2020,08,26,00,00,00)
  ch_el=23.7;ch_az=346.3;
  warning('GUISDAP:guispert','Setting azimuth to 346.3 deg and elevation to 23.7 deg.')
% Incorrect values in the data for the pointing direction in Kiruna.
end

if strcmp(name_ant(1:3),'kir') & d_date>=datenum(2021,08,11,00,00,00) & d_date<datenum(2021,08,19,00,00,00)
  ch_range=ch_range/1000;
% Incorrect value in the parameter block for the intersection range in Kiruna.
end

warning('off','GUISDAP:guispert')