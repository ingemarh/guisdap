function par=iri(fields,tim,pos,h)
% INPUT:  pos(lat,long)  LATITUDE NORTH AND LONGITUDE EAST IN DEGREES 69.2,19.2
% 	  tim(tsec,year) eg [0 1996] for 960101 00:00 ut
%         H(hstart,hstop,hstep)  HEIGHT RANGE IN KM; maximal 100 heights 100,590
%
%   	  fields 1  ELECTRON DENSITY/M-3     [1 4 3]
%   		2  NEUTRAL TEMPERATURE/K
%   		3  ION TEMPERATURE/K
%   		4  ELECTRON TEMPERATURE/K
%   		5  O+ ION DENSITY/M-3
%   		6  H+ ION DENSITY/M-3
%   		7  HE+ ION DENSITY/M-3
%   		8  O2+ ION DENSITY/M-3
%   		9  NO+ ION DENSITY/M-3
%              12  HEIGHTS
%*****************************************************************
%*** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        ***
%***     ELECTRON DENSITY         60/80 KM       1000 KM       ***
%***     TEMPERATURES              120 KM        3000 KM       ***
%***     ION DENSITIES             100 KM        1000 KM       ***
%*****************************************************************
if nargin<1, fields=[]; end
if nargin<2, tim=[]; end
if nargin<3, pos=[]; end
if nargin<4, h=[]; end
if isempty(fields), fields=[1 4 3]; end
if isempty(tim), tim=[15552090 1996]; end
if isempty(pos), pos=[69.2 19.2]; end
if isempty(h), h=[100 590 0]; end
if h(3)==0, h(3)=(h(2)-h(1))/99; end
len=(h(2)-h(1))/h(3)+1;

iri_m=IRI2012();
JF = IRI2012.defaultIRIswitches();
JF(IRI2012.MESSAGES_ON_SW)=false;

JMAG = IGRF.GEOGRAPHIC_COORDINATES;
id=-tim(1)/86400.;
ut=tim(1)/3600.+id*24;
iy=round(tim(2));
[outf,oarr,iri_m] = iri_m.IRI_SUB(JF,JMAG,pos(1),pos(2),iy,id-1,ut+25,h(1),h(2),h(3));
par=outf(fields,:)';
d=find(fields==12); if length(d)==1, par(:,d)=h(1)+h(3)*(0:len-1)'; end
d=find(par==-1); if length(d), par(d)=NaN; end
