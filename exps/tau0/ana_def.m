% Analysis defaults
global iono_model
 iono_model='iri';
%global fit_altitude
% fit_altitude=[0 Inf;0000 Inf;107 1000;0 107;0 Inf;500 Inf;0 0];

% Ingemar orig gating
%first=100; last=1300; d1=4; d2=2;
%altd=[0 d1:d2:(sqrt((last-first)*2*d2))];
%analysis_altit=first+cumsum(altd);
%analysis_maxwidth=[altd(2:end) altd(end)];
d=find(analysis_altit<1300);
analysis_altit=analysis_altit(d);
analysis_maxwidth=analysis_maxwidth(d);

% mule tau0 analys gates
%analysis_altit=[90:3:150 150:6:204 204:12:330 330:36:1100]*sin(81*pi/180);
