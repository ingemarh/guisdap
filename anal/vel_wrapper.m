% function vel_wrapper(scan,dirs)
% Wrapper to make default velocity vectors and plots for CPs
% Inputs:scan	'ip2e'	Scan type, defined are:
%			ip2e for monostatic ESR ip2 scan
%			ip2t for monostatic UHF ip2 scan
%			ip2kst for tristatic UHF ip2 scan
%			cp2 for monostatic cp2 scan
%			cp2kst for tristatic UHF ip2 scan
%			cp1 for tristatic UHF cp1 scan
%			cp3kst for tristatic UHF cp3 scan
%			cluster for overlapped ESR and VHF runs
%	dirs		{result_path} Directories containing data (wild cards accepted)
%				Sites separated in cell
% See also VECTOR_VELOCITY, EFIELD
%
function vel_wrapper(scan,dirs)
global result_path
ld=[]; uperr=[]; plots={'Vm'}; ptype='t';
if nargin<1
 fprintf('Scans defined: ip2e ip2t ip2kst cp2kst cp3kst cp1 cp1kst cp2 cluster\n')
 scan=minput('Choose',[],1);
end
if nargin<2
 nd=0;
 while nd<1 | ~isempty(dirs{nd})
  nd=nd+1;
  dirs(nd)={minput(sprintf('Data directory no %d',nd),[],1)};
 end
 dirs=dirs(1:nd-1);
end
switch scan
 case 'ip2e'
  alt=[90 110 130 160 400]; td=180; plots={'Vg','Vg',[],'Vm'};
 case 'ip2t'
  alt=[90 100 110 130 160 500]; td=4*240; plots={'Vg','Vg','Vg',[],'Vm'};
 case {'ip2kst' 'cp2kst'}
  alt=[170 450]; td=1;
 case {'cp3kst'}
  alt=[170 450]; td=1; ptype='p';
 case {'cp1' 'cp1kst'}
  alt=[170 500]; td=1;
 case 'cp2'
  alt=[90 100 110 130 160 500]; td=360; plots={'Vg','Vg','Vg',[],'Vm'};
 case 'cluster'
  alt=[160 500]; td=120; uperr=1; ld=50:.5:90; ptype='p';
 otherwise
  error('GUISDAP:default','No such scan defined: %s',scan)
end
r=vector_velocity(dirs,alt,td,ld,uperr,[],fullfile(result_path,'vectors'));
np=length(plots);
ntp=[];
for i=1:np, if ~isempty(plots{i}), ntp=[ntp i]; end, end
np=length(ntp);
if strcmp(ptype,'t')
 npc=1; npr=np;
 if np==1
  orient rotated
 else
  orient tall
  sq=np^2;
 end
else
 npc=floor(sqrt(np)); npr=ceil(np/npc);
 sq=sqrt(8);
 orient tall
end
for i=1:np
 subplot(npr,npc,np+1-i)
 efield(r,[plots{ntp(i)} ptype],alt((0:1)+ntp(i)),[],2)
 if strcmp(ptype,'t')
  if alt(ntp(i))<150, ylim=1000; else, ylim=2000; end
  set(gca,'ylim',[-ylim ylim])
  if i>1, xlabel([]), end
 end
end
% Squeeze things before printing
if strcmp(ptype,'p') | np>1
 gc=get(gcf,'children');
 pos=get(gc,'pos');
 for i=1:length(gc)
  p=pos{i};
  sh=(0.5-(p(2)+p(4)/2))/sq;
  set(gc(i),'pos',[p+[0 sh 0 0]])
 end
 if strcmp(ptype,'p') | np>1
  gl=findobj(gcf,'visible','off','xlim',[0 202]);
  set(gl,'pos',get(gl,'pos')+[.1 0 0 0])
 end
end
print('-deps2',r)
print('-dpng256',r)
if strcmp(ptype,'p') | np>1
 for i=1:length(gc)
  set(gc(i),'pos',pos{i})
 end
end
fprintf('%s.mat .eps .png produced\n',r)
