% NCAR_output.m
%
% Optionally:
% Writes profile of results in NCAR binary and/or ascii format to a 
% file ready to read into MADRIGAL, etc. 
%--------------------------------------------------------------------------
%     COPYRIGHT 1997-2003 EISCAT SCIENTIFIC ASSOCIATION.
%     PERMISSION TO USE, COPY, AND DISTRIBUTE, FOR NON-COMMERCIAL PURPOSES,
%     IS HEREBY GRANTED WITHOUT FEE, PROVIDED THAT THIS LICENSE INFORMATION
%     AND COPYRIGHT NOTICE APPEAR IN ALL COPIES.
%--------------------------------------------------------------------------
%
function NCAR_output(matfile,NCAR_ascii,NCAR_binary)
global NCAR_fid

if nargin==0
  if ~isempty(NCAR_fid) & any(NCAR_fid)
    fclose(NCAR_fid(find(NCAR_fid)));
  end
  NCAR_fid=[]; return
elseif isempty(NCAR_fid)
  NCAR_fid=[0 0];
% output files.  Set any file to zero length to disable that output
  if nargin<3, NCAR_binary=[]; end
  if nargin<2, NCAR_ascii=[]; end
  if ~isempty(NCAR_ascii)
    NCAR_fid(1)=fopen(NCAR_ascii,'a');
  end
  if ~isempty(NCAR_binary)
    NCAR_fid(2)=fopen(NCAR_binary,'a','b');
  end
elseif ~NCAR_fid(1) & ~isempty(NCAR_ascii)
  NCAR_fid(1)=fopen(NCAR_ascii,'w');
end

load(matfile)
% set up absent data value
AbsentData		=-32767;

% set up NCAR single-valued parameters
%spar=[Azimuth,Elevation,Scattering_half_angle,System_temperature,...
%    Peak_transmitted_power]
sparcod=[130 140 190 482 486];

% set up NCAR multi-valued parameters
%mvarcod=[Altitude(km),Alt(dm),Status,Resid,O+/Ne,...
%       Log10_Ne,Ti,TeTi,CollFreq,Vi,...
%     errors...				];
mvarcod=[110 111 430 420 620 520  550  570  720  580,...
                            -520 -550 -570 -720 -580];

KINDAT	=6000;
switch name_site
  case 'L', KINST=95;	% EISCAT Svalbard Radar
            KINDAT=6800;
  case 'K', KINST=71;	% EISCAT Kiruna UHF
  case 'T', KINST=72;	% EISCAT Tromso UHF
  case 'S', KINST=73;	% EISCAT Sodankyla UHF
  case 'V', KINST=74;	% EISCAT Tromso VHF
  otherwise, KINST=AbsentData;
             KINDAT=AbsentData;
end

LPROL=16;		% length of prologue
JPAR =length(sparcod);	% # single-valued parameters
MPAR =length(mvarcod);	% # multi-valued parameters	
NROW =length(r_h);	% # entries for each multi-valued parameter

% the prologue
ITIM =[r_time(:,1) r_time(:,[2 4])*100+r_time(:,[3 5]) r_time(:,6)*100];
prol=fix([KINST KINDAT ITIM(1,:) ITIM(2,:) LPROL JPAR MPAR NROW]);

% codes and values for the single-valued parameters
if r_el>90	% Cast azimuth and elevation into range +-180, 0-90 degrees
  m_el=180-r_el;
  m_az=r_az+180;
else
  m_el=r_el;
  m_az=r_az;   
end
m_az=mod((m_az+360),360);
if m_az>180
  m_az=m_az-360;
end
if exist('r_Tsys')
  tsys=median(r_Tsys);
else
  tsys=AbsentData;
end
spar=[sparcod;round([m_az*100 m_el*100 r_SCangle*180/pi tsys r_Pt/1000])];

% Assemble output vector of codes and values for the multi-valued parameters
var=real([log10(r_param(:,1))*1e3 r_param(:,2) r_param(:,3)*1e3 log10(r_param(:,4))*1e3 -r_param(:,5)]);
evar=real([log10(r_error(:,1))*1e3 r_error(:,2) r_error(:,3)*1e3 log10(r_param(:,4))*1e3 r_error(:,5)]);
mvar=var;
% only output results for fit parameter 0
d=find(r_status~=0); var(d,:)=AbsentData; evar(d,:)=AbsentData;
d=find(r_error(:,1:5)==0); evar(d)=AbsentData; var(d)=mvar(d);
pos=[fix(r_h) rem(r_h,1)*1e4];
mvar=round([pos r_status [r_res(:,1) r_dp]*1e3 var evar]);
% replace infinities and all values > 32767
mvar(find(~isfinite(mvar) | abs(mvar)>-AbsentData))=AbsentData;
Multivar=[mvarcod;mvar];
 
if NCAR_fid(1)
  KREC=1101;	% kind of record (data, character version)
  LTOT=4+NROW;	% number of lines in record
% LTOT=1+fix((JPAR+19)/20)*2+fix((MPAR+19)/20)*(NROW+1);
  fprintf(NCAR_fid(1),'%6i',[LTOT KREC prol]);
  fprintf(NCAR_fid(1),'\n');
  for i=1:2
    fprintf(NCAR_fid(1),'%6i',spar(i,:));
    fprintf(NCAR_fid(1),'\n');
  end
  for i=1:NROW+1
    fprintf(NCAR_fid(1),'%6i',Multivar(i,:));
    fprintf(NCAR_fid(1),'\n');
  end
end

if NCAR_fid(2)
  KREC=1002;				% kind of record (data, binary version)
  LTOT=LPROL+2*JPAR+MPAR*(NROW+1);	% number of entries
  fwrite(NCAR_fid(2),[LTOT KREC prol row(spar') row(Multivar')],'short');
end

if findstr('TVL',name_site) & ~isempty(r_pp)
%power profiles
 mvarcod=[120 121 505];
 KINDAT=KINDAT+1;
 MPAR=length(mvarcod);
 NROW=length(r_pprange);
 prol([2 13 14])=[KINDAT MPAR NROW];
 var=real(log10(r_pp(:,1))*1e3);
 pos=[fix(r_pprange) rem(r_pprange,1)*1e4];
 mvar=round([pos var]);
 mvar(find(~isfinite(mvar) | abs(mvar)>-AbsentData))=AbsentData;
 Multivar=[mvarcod;mvar];
 if NCAR_fid(1)
  KREC=1101;
  LTOT=4+NROW;
  fprintf(NCAR_fid(1),'%6i',[LTOT KREC prol]);
  fprintf(NCAR_fid(1),'\n');
  for i=1:2
    fprintf(NCAR_fid(1),'%6i',spar(i,:));
    fprintf(NCAR_fid(1),'\n');
  end
  for i=1:NROW+1
    fprintf(NCAR_fid(1),'%6i',Multivar(i,:));
    fprintf(NCAR_fid(1),'\n');
  end
 end
 if NCAR_fid(2)
  KREC=1002;
  LTOT=LPROL+2*JPAR+MPAR*(NROW+1);
  fwrite(NCAR_fid(2),[LTOT KREC prol row(spar') row(Multivar')],'short');
 end
end
