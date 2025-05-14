function [az,el]=solar_angle(lat,lon,ut)
d=datevec(ut);
d0=datenum([d(1),1,1]);
lat=lat*pi/180;
N=ut-d0; %day number

hloc=rem(N*24,24)+lon/15; %local time;
h=(hloc-12)*15; %hour angle
d=asin(sin(-23.44*pi/180)*cos(2*pi*(N+10)/365.24+2*0.0167*sin(2*pi*(N-2)/365.24))); %sun declination
za=acos(sin(lat)*sin(d)+cos(lat)*cos(d)*cos(h*pi/180)); %sun zenith angle
el=90-za*180/pi;

%az=asin(-sin(h*pi/180)*cos(d)/sin(za))*180/pi
az=acos((sin(d)-cos(za)*sin(lat))/(sin(za)*cos(lat)))*180/pi;
if h>0, az=360-az; end
