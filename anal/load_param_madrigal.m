function [Time,par2D,par1D,rpar2D,err2D]=load_param_madrigal(data_path,fileno,do_err)
% Function to read the plasma parameters from madrigal NCAR files
%
% [Time,par2D,par1D,rpar2D]=load_param_madrigal(data_path,status)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys]
% rpar2D [Ran,Alt,Ne]

global name_expr r_RECloc name_ant local r_XMITloc
ask=0; wget=1;
Time=[]; par2D=[]; par1D=[]; rpar2D=[]; err2D=[];
if nargin<2, fileno=[]; end
if nargin<3, do_err=0; end
if isempty(fileno), ask=1; fileno=1; end

of=local.tfile;
REClocs=[67.863 20.44 .412;69.583 19.21 .030;67.367 26.65 .180;69.583 19.21 .030;78.153 16.029 .438];
XMITlocs=[ones(4,1)*[69.583 19.21 .030];78.153 16.029 .438];
if exist(data_path,'file')
 global path_GUP
 cmd2=[' ' path_GUP '/bin/madDataDisplay >'];
 if ~exist(cmd2(2:end-2)), return, end
 ws=''; cmd3=''; cmd1=''; cmd4='QUERY_STRING=';
 antennas={'kir' 'uhf' 'sod' 'vhf' 'esr' '32m' '42m'};
 [devnull,filename]=fileparts(data_path); ll=16;
 ldp=length(filename); ll=16;
 name_expr='unknown'; name_ant='unk';
 if strfind(filename,'NCAR') && ldp>ll
  at=strfind(filename(ll+1:end),'@');
  if isempty(at)
   at=strfind(filename(ll+1:end),'_');
  end
  if at
   name_expr=filename(16+[1:at(end)-1]);
   name_ant=filename(16+at(end)+1:end);
  end
 end
 i=fix((strfind(cell2mat(antennas),name_ant(1:3))+2)/3);
 if i<1 || i>7, i=1; elseif i>5, i=5; end
 r_RECloc=REClocs(i,:);
 r_XMITloc=XMITlocs(i,:);
 hl=10;
else
 [wget,devnull]=system('which curl');
 if wget
  cmd1='wget'; cmd2=' -O '; cmd3=' '; cmd4=' --post-data=';
 else
  cmd1='curl'; cmd2=' -o '; cmd3=' -f '; cmd4=' -d ';
 end
 website='http://portal.eiscat.se/madrigal/';
 ws=['''' website 'experiments/' data_path '/expTab.txt'''];
 [mad,devnull]=system([cmd1 cmd2 of cmd3 ws]);
 if mad
  return
 end
 [name_expr,kinst]=textread(of,'%*s%*s%s%*s%*s%*s%*s%*s%d%*[^\n]','delimiter',',');
 delete(of)
 name_expr=char(name_expr);
 name_expr(strfind(name_expr,'_'))=[];
 name_expr(strfind(name_expr,' '))=[];
 ws=['''' website 'experiments/' data_path '/fileTab.txt'''];
 [mad,devnull]=system([cmd1 cmd2 of cmd3 ws]);
 if mad
  return
 end
 filename=textread(of,'%s%*[^\n]','delimiter',',');
 delete(of)
 antennas={'kir' 'uhf' 'sod' 'vhf' 'esr'};
 i=min(kinst-70,5);
 if i<1 || i>5
  r_RECloc=REClocs(1,:);
  r_XMITloc=XMITlocs(i,:);
 else
  name_ant=char(antennas(i)); r_RECloc=REClocs(i,:); r_XMITloc=XMITlocs(i,:);
 end
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
 data_path=['/opt/madrigal/experiments/' data_path '/' filename];
 ws=['''' website 'cgi-bin/madDataDisplay'''];
 hl=8;
end

if do_err
%param='UT1 UT2 AZM ELM GDALT GDLAT GLON RANGE CHISQ POWER SYSTMP CO DCOL NEL DNEL TE DTE TI DTI VO DVO VOBI DVOBI PO+';
 param='UT1 UT2 GDALT RANGE AZM ELM GDLAT GLON CHISQ SYSTMP POWER NEL DNEL TI DTI TE DTE VO DVO VOBI DVOBI CO DCOL PO+';
else
%param='UT1 UT2 AZM ELM GDALT GDLAT GLON RANGE CHISQ POWER SYSTMP CO NEL POPL TE TI VO VOBI PO+';
 param='UT1 UT2 GDALT RANGE AZM ELM GDLAT GLON CHISQ SYSTMP POWER POPL NEL TI TE VO VOBI CO PO+';
end
t2=clock;
arg=sprintf('fileName=%s&startYear=1981&endYear=%d&startMonth=1&startDay=1&endMonth=12&endDay=31&parmlist=%s&header=f&assumed=0&badval=NaN&mxchar=9999&state=text',data_path,t2(1),param);
for i=fliplr(find(arg=='&' | arg==' '))
 arg=[arg(1:i-1) '\' arg(i:end)];
end

[mad,devnull]=system([cmd1 cmd4 arg cmd2 of cmd3 ws]);
if mad
 return
end
%pwant=strread(param,'%s');
%pread=textread(of,'%s',length(pwant),'headerlines',hl-1);
data=textread(of,'','headerlines',hl);
delete(of)
if isempty(data)
 return
end

%vec=[3 4 10 11];
rvec=[5 6 11 10];
%vec=[8 5 13 15 16 17 12 19 9];
avec=[4 3 13 15 14 16 18 19 9];
pp=[];
if do_err
  data(:,12:13)=10.^data(:,12:13);
% avec=[8 5 14 16 18 20 12 24 9];
  avec=[4 3 12 16 14 18 22 24 9];
% evec=[15 17 19 21 13];
  evec=[13 17 15 23 23];
else
  %vec=[8 5 14];
  pvec=[4 3 12];
  data(:,12:13)=10.^data(:,12:13);
  evec=[];
  pp=find(isfinite(data(:,pvec(3))));
end
npar2D=length(avec);
nrpar2D=3;
nerr2D=npar2D-4;
ppres=.25; % pp resolution (km)
acres=.25; % acf resolution (km)

acf=find(isfinite(data(:,avec(3))));
[Time,ia,ja]=unique(data(acf,1:2),'rows');
n_tot=size(Time,1);
n_alt=max(diff(ia));
Time=datenum(1950,1,1)+Time'/86400;

par1D=data(acf(ia),rvec);
par1D(:,1)=mod(par1D(:,1)+360,360); %0-360 degrees
par1D(:,3)=par1D(:,3)/10; %10 kW
par2D=ones(n_alt,n_tot,npar2D)*NaN;
naalt=0;
n_ralt=0;
nralt=0;

if nargout>3
 if do_err
  err2D=ones(n_alt,n_tot,nerr2D)*NaN;
 elseif ~isempty(pp)
  [dum,ip,jp]=unique(data(pp,1:2),'rows');
  if size(dum,1)==n_tot
   n_ralt=max(diff(ip));
   rpar2D=ones(n_ralt,n_tot,3)*NaN;
  else
   warning('GUISDAP:default','Different pp and acf data times not allowed --ignoring pp')
  end
 end
end
if ~sum(isfinite(data(:,avec(6))))
 avec(6)=avec(6)+1;
 if do_err
  avec(6)=avec(6)+1; evec(4)=evec(4)+2;
 end
end
for i=1:n_tot
  ppp=data(acf(find(ja==i)),avec);
  [ppr,k,l]=unique(round(ppp(:,1)/acres));
  nalt=size(ppr,1); ppar=ones(nalt,npar2D);
  for n=1:nalt
    ppar(n,:)=mean(ppp(find(l==n),:),1);
  end
  par2D(1:nalt,i,:)=ppar;
  naalt=max(naalt,nalt);
  if n_ralt>0
    ppp=data(pp(find(jp==i)),pvec);
    [ppr,k,l]=unique(round(ppp(:,1)/ppres));
    nalt=size(ppr,1); ppar=ones(nalt,3);
    for n=1:nalt
      ppar(n,:)=mean(ppp(find(l==n),:),1);
    end
    rpar2D(1:nalt,i,:)=ppar;
    nralt=max(nralt,nalt);
  elseif do_err
    ppp=data(acf(find(ja==i)),evec);
    ppar=ones(nalt,nerr2D);
    for n=1:nalt
      ppar(n,:)=mean(ppp(find(l==n),:),1);
    end
    err2D(1:nalt,i,:)=ppar;
  end
end
if naalt<n_alt
  par2D=par2D(1:naalt,:,:);
  if do_err
    err2D=err2D(1:naalt,:,:);
  end
end
if nralt<n_ralt
  rpar2D=rpar2D(1:nralt,:,:);
end
