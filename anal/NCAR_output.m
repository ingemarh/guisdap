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
  if ~isempty(NCAR_fid) && any(NCAR_fid)
    for fid=NCAR_fid(find(NCAR_fid)), fclose(fid); end
  end
  NCAR_fid=[]; return
elseif isempty(NCAR_fid) || all(NCAR_fid<1)
  NCAR_fid=[0 0];
% output files.  Set any file to zero length to disable that output
  if nargin<3, NCAR_binary=[]; end
  if nargin<2, NCAR_ascii=[]; end
  if ~isempty(NCAR_ascii)
    NCAR_fid(1)=fopen(NCAR_ascii,'w');
  end
  if ~isempty(NCAR_binary)
    NCAR_fid(2)=fopen(NCAR_binary,'w','b');
  end
elseif ~NCAR_fid(1) && ~isempty(NCAR_ascii)
  NCAR_fid(1)=fopen(NCAR_ascii,'w');
end

load(matfile)
% set up absent data values
AbsentData=-32767;
ModelData=-32766;
BadData=-32767; % madrigal cannot handle +32767

KINDAT=6800;
if exist('Vdate','var')
 switch name_ant
  case 'esr', KINST=95;
  case '32m', KINST=95;
  case '42m', KINST=95;
  case 'uhf', KINST=72;
  case 'vhf', KINST=74;
  case 'kir', KINST=71; if r_time(1)>2012, KINST=75; end
  case 'sod', KINST=73; if r_time(1)>2012, KINST=76; end
  case 'esa', KINST=70;
  case 'quj', KINST=100; KINDAT=8800;
  otherwise, KINST=AbsentData;
 end
 sparcod=[]; name_site=[];
 ITIM=zeros(2,4);
else
 mvel=580;
 switch name_site
  case 'L', KINST=95; % EISCAT Svalbard Radar
  case 'K', KINST=71; mvel=590; % EISCAT Kiruna UHF
            if r_time(1)>2012, KINST=75; end
  case 'T', KINST=72; % EISCAT Tromso UHF
  case 'S', KINST=73; mvel=590; % EISCAT Sodankyla UHF
            if r_time(1)>2012, KINST=76; end
  case 'V', KINST=74; % EISCAT Tromso VHF
  case 'Q', KINST=100; KINDAT=8800; % Quing IS radar
  otherwise, KINST=AbsentData;
             KINDAT=AbsentData;
 end
% set up NCAR single-valued parameters
%spar=[Azimuth,Elevation,Scattering_half_angle,System_temperature,...
%    Peak_transmitted_power]
 sparcod=[130 140 190 482 486];
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
 spar=[sparcod;round([[m_az m_el r_SCangle*180/pi]*100 tsys r_Pt/1000])];
 ITIM=[r_time(:,1) r_time(:,[2 4])*100+r_time(:,[3 5]) r_time(:,6)*100];
end
prol=fix([KINST KINDAT ITIM(1,:) ITIM(2,:) 0 0 0 0]); %the prologe

if exist('r_param') && ~isempty(r_param)
 % set up NCAR multi-valued parameters
 %mvarcod=[Altitude(km),Alt(dm),Status,Resid,O+/Ne,...
 %       Log10_Ne,Ti,TeTi,CollFreq,Vi,...
 %     errors...				];
 mvarcod=[110 111 430 420 620 520  550  570  720 mvel,...
                            -520 -550 -570 -720 -mvel];
 % Assemble output vector of codes and values for the multi-valued parameters
 var=real([log10(r_param(:,1))*1e3 r_param(:,2) r_param(:,3)*1e3 log10(r_param(:,4))*1e3 -r_param(:,5)]);
 evar=real([log10(r_error(:,1))*1e3 r_error(:,2) r_error(:,3)*1e3 log10(r_error(:,4))*1e3 r_error(:,5)]);
 mvar=var;
 % only output results for fit parameter 0
 d=find(r_status~=0); var(d,:)=BadData; evar(d,:)=BadData;
 d=find(r_error(:,1:5)==0); evar(d)=ModelData; var(d)=mvar(d);
 pos=[fix(r_h) rem(r_h,1)*1e4];
if ~isempty(strfind('TVLQ',name_site)) && exist('r_w')
  mvarcod=[mvarcod(1:2) 115 116 mvarcod(3:end)];
  Earth_radius=6372; sin_el=sin(pi*r_el/180);
  ran2=(r_range+r_w(:,1)/2)/Earth_radius; ran1=(r_range-r_w(:,1)/2)/Earth_radius;
  hw=Earth_radius*(sqrt(1+2*ran2*sin_el+ran2.^2)-sqrt(1+2*ran1*sin_el+ran1.^2));
  pos=[pos fix(hw) rem(hw,1)*1e4];
 end
 mvar=round([pos r_status [r_res(:,1) r_dp]*1e3 var evar]);
 NCAR_write(NCAR_fid,KINDAT,prol,spar,mvarcod,mvar,AbsentData,BadData)
end

 if ~isempty(strfind('TVLQ',name_site)) & exist('r_pp') & ~isempty(r_pp)
%power profiles
 mvarcod=[120 121 505];
 var=real(log10(r_pp(:,1))*1e3);
 pos=[fix(r_pprange) rem(r_pprange,1)*1e4];
 if exist('r_pperr') & exist('r_ppw')
  mvarcod=[120 121 125 126 505 -505];
  pos=[pos fix(r_ppw) rem(r_ppw,1)*1e4];
  var=[var real(log10(r_pperr(:,1))*1e3)];
 end
 mvar=round([pos var]);
 NCAR_write(NCAR_fid,KINDAT+1,prol,spar,mvarcod,mvar,AbsentData,BadData)
end

if exist('Vdate','var')
%vector velocities
 %mvarcod=[Lat(ddegN),Long(ddegE),Alt(km),VE,VN,VU,errors]
  mvarcod=[160 170 110 1210 1220 1230 -1210 -1220 -1230];
  t1all=unique(sort(Vdate(1,:)));
  for t1=t1all
    d=find(Vdate(1,:)==t1);
    t2all=unique(sort(Vdate(2,d)));
    r_time=datevec(t1);
    prol(3:6)=[r_time(1) r_time([2 4])*100+r_time([3 5]) fix(r_time(6)*100)];
    for t2=t2all
      d1=d(find(Vdate(2,d)==t2));
      r_time=datevec(t2);
      prol(7:10)=[r_time(1) r_time([2 4])*100+r_time([3 5]) fix(r_time(6)*100)];
      pos=Vpos(d1,1:2); dp=find(pos>180); pos(dp)=pos(dp)-360;
      mvar=round([pos*100 Vpos(d1,3) Vg(d1,:) sqrt(Vgv(d1,1:3))]);
      NCAR_write(NCAR_fid,KINDAT,prol,[],mvarcod,mvar,AbsentData,BadData)
    end
  end
end
return

function NCAR_write(NCAR_fid,KINDAT,prol,spar,mvarcod,mvar,AbsentData,BadData)
LPROL=length(prol)+2;	% length of prologue
JPAR=size(spar);	% # single-valued parameters
NROW=size(mvar,1);	% # entries for each multi-valued parameter
MPAR=length(mvarcod);	% # multi-valued parameters
prol([2 11:14])=[KINDAT LPROL JPAR(2) MPAR NROW];
% replace infinities and all values > 32767
mvar(find(~isfinite(mvar) | abs(mvar)>-AbsentData))=BadData;
Multivar=[mvarcod;mvar];
if NCAR_fid(1)
  KREC=1101;	% kind of record (data, character version)
  LTOT=2+NROW+JPAR(1);	% number of lines in record
% LTOT=1+fix((JPAR+19)/20)*2+fix((MPAR+19)/20)*(NROW+1);
  fprintf(NCAR_fid(1),'%6i',[LTOT KREC prol]);
  fprintf(NCAR_fid(1),'\n');
  for i=1:JPAR(1)
    fprintf(NCAR_fid(1),'%6i',spar(i,:));
    fprintf(NCAR_fid(1),'\n');
  end
  for i=1:NROW+1
    fprintf(NCAR_fid(1),'%6i',Multivar(i,:));
    fprintf(NCAR_fid(1),'\n');
  end
end
if NCAR_fid(2)
  KREC=1002;	% kind of record (data, binary version)
  LTOT=LPROL+2*JPAR(2)+MPAR*(NROW+1);	% number of entries
  fwrite(NCAR_fid(2),[LTOT KREC prol row(spar') row(Multivar')],'short');
end
return
