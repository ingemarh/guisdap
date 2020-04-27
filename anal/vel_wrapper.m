% function vel_wrapper(scan,dirs,override)
% Wrapper to make default velocity vectors and plots for CPs
% Inputs:scan	Scan type, defined are:
%		ip2e for monostatic ESR ip2 scan
%		ip3 for monostatic ESR ip3 scan
%		ip2t for monostatic UHF ip2 scan
%		ip2kst for tristatic UHF ip2 scan
%		cp2 for monostatic cp2 scan
%		cp2kst for tristatic UHF ip2 scan
%		cp1kst for tristatic UHF cp1 scan
%		cp3kst for tristatic UHF cp3 scan
%		cluster for overlapped ESR and VHF beams
%	dirs	{result_path} Directories containing data (wild cards accepted)
%			Sites separated in cell
% See also VECTOR_VELOCITY, EFIELD
%
function vel_wrapper(scan,dirs,override)
if nargin<1, scan=[]; end
if nargin<2, dirs=[]; end
if nargin<3, override=[]; end
global result_path
ld=[]; uperr=[]; plots={'Vm'}; ptype='t'; ylim=[2000 2000]; dynavel=0;
e=[90 100 107.5 112.5 117.5 122.5 130]; %from old cp1
%e=90:10:130;
f=[160 500];
if isempty(scan)
 fprintf('Scans defined: ip2e ip2t ip2kst cp2kst cp3kst cp3kstl ip3 cp1 cp1kst lowel cp2 cluster\n')
 scan=minput('Choose',[],1);
end
if isempty(dirs)
 nd=0;
 while nd<1 || ~isempty(dirs{nd})
  nd=nd+1;
  dirs{nd}=minput(sprintf('Data directory no %d',nd),[],1);
 end
 dirs=dirs(1:nd-1);
end
switch scan
 case 'ip2e'
  alt=[e f]; td=180;
  plots(2:length(alt)-3)={'Vg'}; plots([1 end+2])={[] 'Vm'};
 case 'ip2t'
  alt=[e f]; td=240;
  plots(2:length(alt)-3)={'Vg'}; plots([1 end+2])={[] 'Vm'};
 case {'ip2kst' 'cp2kst'}
  alt=f; td=1;
 case {'cp3kst'}
  alt=f; td=1; ptype='p'; uperr=50;
 case {'cp3kstl'}
  alt=f; td=1440.; ptype='p'; uperr=50; ld=60:.5:90;
 case {'ip3'}
  alt=[e f]; td=1440./[1 2]; uperr=50; ld=60:.5:90; ptype='p';
  plots(2:length(alt)-3)={'Vg'}; plots([1 end+2])={[] 'Vm'};
  case {'cp1' 'cp1kst' 'lowel'}
    % all tristatic cases should work with these settings
  alt=f; td=1;
 case {'cp2' 'cp2t' 'cp2e'}
  alt=[e f]; td=360;
  plots(2:length(alt)-3)={'Vg'}; plots([1 end+2])={[] 'Vm'};
 case 'cluster'
  alt=f; td=120; uperr=1; ld=50:.5:90; ptype='p';
 otherwise
  alt=minput('Altitude ranges',[e f]);
  td=minput('Time interval',180);
  ld=minput('Latitude intervals',ld);
  uperr=minput('Uperr',uperr);
  dynavel=minput('Dynasonde velocities',dynavel);
  plots(1:length(alt)-3)={'Vg'}; plots([end+[1 2]])={'' 'Vm'};
  plots=minput('Plots',plots);
  ptype=minput('Plot type',ptype,1);
end
if ~isempty(override)
 eval(override)
end
r=vector_velocity(dirs,alt,td,ld,uperr,[],fullfile(result_path,'vectors'),dynavel);
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
 %sq=sqrt(8);
 sq=3*(npr-1.5).^2+2;
 orient tall
end
for i=1:np
 subplot(npr,npc,np+1-i,'align')
 efield(r,[plots{ntp(i)} ptype],alt((0:1)+ntp(i)),[],2)
 if strcmp(ptype,'t')
  if i>1, delete(get(gca,'xlabel')), end
  set(gca,'ylim',ylim(1+(alt(ntp(i))<150))*[-1 1])
 end
end
% Squeeze things before printing
if strcmp(ptype,'p') || np>1
 gc=get(gcf,'children');
 pos=get(gc,'pos');
 for i=1:length(gc)
  p=pos{i};
  sh=(0.5-(p(2)+p(4)/2))/sq;
  set(gc(i),'pos',[p+[0 sh 0 0]])
 end
 if strcmp(ptype,'p')
  gl=findobj(gcf,'visible','off','xlim',[0 202]);
  if ~isempty(gl), set(gl,'pos',get(gl,'pos')+[.1 0 0 0]), end
 end
end
print('-dpdf',r)
print('-dpng256',r)
if strcmp(ptype,'p') || np>1
 for i=1:length(gc)
  set(gc(i),'pos',pos{i})
 end
end

fprintf('%s.mat .pdf .png produced\n',r)

if exist(r)
 rmdir(r,'s')
end
mkdir(r)

[namepath,namefile] = fileparts(r);
EISCAThdf5file = fullfile(namepath,['EISCAT_' namefile],['EISCAT_' namefile '.hdf5']);
store_image2Hdf5([r '.png'],EISCAThdf5file);
strds2hdf5(EISCAThdf5file,'/metadata','figure_links',{[namefile '.pdf']})

filelist = dir(fullfile(namepath,['*' namefile '*']));
for i =1:length(filelist)
 if ~strcmp(fullfile(filelist(i).folder,filelist(i).name),r) 
  movefile(fullfile(filelist(i).folder,filelist(i).name),r) 
 end
end

end

