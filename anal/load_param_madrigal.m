function [Time,par2D,par1D,rpar2D]=load_param_madrigal(data_path,fileno)
% Function to read the plasma parameters from madrigal NCAR files
%
% [Time,par2D,par1D,rpar2D]=load_param_madrigal(data_path,status)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys]
% rpar2D [Ran,Alt,Ne]

global name_expr r_RECloc name_ant
ask=0;
Time=[]; par2D=[]; par1D=[]; rpar2D=[];
if nargin<2, fileno=[]; end
if isempty(fileno), ask=1; fileno=1; end

website='http://www.eiscat.se/madrigal/';
of=tempname;
ws=['''' website 'experiments/' data_path '/expTab.txt'''];
%if unix(['curl -o ' of ' -f ' ws ' 2>/dev/null'])
if unix(['wget -O ' of ' ' ws ' 2>/dev/null'])
 return
end
[name_expr,kinst]=textread(of,'%*s%*s%s%*s%*s%*s%*s%*s%d%*[^\n]','delimiter',',');
delete(of)
name_expr=char(name_expr);
name_expr(strfind(name_expr,'_'))=[];
name_expr(strfind(name_expr,' '))=[];
ws=['''' website 'experiments/' data_path '/fileTab.txt'''];
%if unix(['curl -o ' of ' -f ' ws ' 2>/dev/null'])
if unix(['wget -O ' of ' ' ws ' 2>/dev/null'])
 return
end
filename=textread(of,'%s%*[^\n]','delimiter',',');
delete(of)
antennas={'kir' 'uhf' 'sod' 'vhf' 'esr'};
REClocs=[67.863 20.44 .412;69.583 19.21 .030;67.367 26.65 .180;69.583 19.21 .030;78.153 16.029 .438];
i=min(kinst-70,5);
if i<1 | i>5, return, end
name_ant=char(antennas(i)); r_RECloc=REClocs(i,:);
if length(filename)>1
 fprintf('Available data files in %s:\n',data_path)
 for i=1:length(filename), fprintf('%d %s\n',i,char(filename(i))), end
 if ask
  fileno=minput('Choose',1);
 end
end
filename=char(filename(fileno));
if strcmp(name_ant,'esr')
 if strfind(filename,'32m'), name_ant='32m';
 elseif strfind(filename,'42m'), name_ant='42m'; end
end
data_path=['/usr/local/madroot/experiments/' data_path '/' filename];

param='UT1,UT2 AZM ELM GDALT GDLAT GLON RANGE CHISQ POWER SYSTMP CO NEL POPL TE TI VO VOBI PO+';
arg=sprintf('fileName=%s&startYear=1981&endYear=2006&startMonth=1&startDay=1&endMonth=12&endDay=31&parmlist=%s&header=f&badval=NaN&mxchar=9999&state=text',data_path,param);
for i=fliplr(find(arg=='&' | arg==' '))
 arg=[arg(1:i-1) '\' arg(i:end)];
end
ws=['''' website 'cgi-bin/madDataDisplay'''];

%if unix(['curl -d ' arg ' -o ' of ' -f ' ws ' 2>/dev/null'])
if unix(['wget --post-data=' arg ' -O ' of ' ' ws ' 2>/dev/null'])
 return
end
data=textread(of,'','headerlines',8);
delete(of)
if isempty(data)
 return
end

acf=find(isfinite(data(:,13)));
pp=find(isfinite(data(:,14)));
[Time,ia,ja]=unique(data(acf,1:2),'rows');
n_tot=size(Time,1);
n_alt=max(diff(ia));
Time=datenum(1950,1,1)+Time'/86400;

npar2D=9;
nrpar2D=3;
ppres=.25; % pp resolution (km)
acres=.25; % acf resolution (km)

par1D=data(acf(ia),[3 4 10 11]);
par1D(:,1)=mod(par1D(:,1)+360,360); %0-360 degrees
par1D(:,3)=par1D(:,3)/10; %10 kW
par2D=ones(n_alt,n_tot,npar2D)*NaN;
naalt=0;
n_ralt=0;
nralt=0;

data(:,13:14)=10.^data(:,13:14);
avec=[8 5 13 15 16 17 12 19 9];
if ~sum(isfinite(data(:,17)))
 avec(6)=18;
end
if nargout>3 & ~isempty(pp)
  [dum,ip,jp]=unique(data(pp,1:2),'rows');
  if size(dum,1)==n_tot
    n_ralt=max(diff(ip));
    rpar2D=ones(n_ralt,n_tot,3)*NaN;
  else
    disp('Different pp and acf data times not allowed --ignoring pp')
  end
end
for i=1:n_tot
  ppp=data(acf(find(ja==i)),avec);
  [ppr,k,l]=unique(round(ppp(:,1)/acres));
  nalt=size(ppr,1); ppar=ones(nalt,9);
  for n=1:nalt
    ppar(n,:)=mean(ppp(find(l==n),:),1);
  end
  par2D(1:nalt,i,:)=ppar;
  naalt=max(naalt,nalt);
  if n_ralt>0
    ppp=data(pp(find(jp==i)),[8 5 14]);
    [ppr,k,l]=unique(round(ppp(:,1)/ppres));
    nalt=size(ppr,1); ppar=ones(nalt,3);
    for n=1:nalt
      ppar(n,:)=mean(ppp(find(l==n),:),1);
    end
    rpar2D(1:nalt,i,:)=ppar;
    nralt=max(nralt,nalt);
  end
end
if naalt<n_alt
  par2D=par2D(1:naalt,:,:);
end
if nralt<n_ralt
  rpar2D=rpar2D(1:nralt,:,:);
end
%disp('Done.')
