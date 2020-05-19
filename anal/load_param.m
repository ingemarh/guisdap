function [Time,par2D,par1D,rpar2D,err2D]=load_param(data_path,status,update)
% Function to read the plasma parameters from GUISDAP result files
%
% [Time,par2D,par1D,rpar2D,err2D]=load_param(data_path,status,update)
% par2D [Ran,Alt,Ne,Te,Ti,Vi,Coll,Comp,Res]
% par1D [Az,El,Pt,Tsys,Oppd/Php]
% rpar2D[Ran,Alt,RawNe]
%  or err2D[Ne,Te,Ti,Vi,Coll]
%
global name_expr r_RECloc name_ant name_strategy r_Magic_const myparams load_apriori rres ppres max_ppw r_XMITloc
global allnames Leap
persistent lastfile
if nargin<3, lastfile=[]; end
if nargin<2, status=[]; end
if isempty(status), status=[0 Inf]; end
if isempty(myparams), myparams=[1 2 4]; end
if isempty(ppres), ppres=.25; end % pp resolution (km)
if isempty(max_ppw), max_ppw=Inf; end % pp resolution (km)
do_rpar=nargout==4;
do_err=nargout==5;

if isempty(strfind(data_path,'*')) && ~isdir(data_path)
  [~,filename,ext] = fileparts(data_path); Leap=[];
  if strcmp(ext,'.hdf5') && strcmp(filename(1:6),'EISCAT')
    [Time,par2D,par1D,rpar2D,err2D]=load_param_hdf5(data_path);
    return
  else
    [Time,par2D,par1D,rpar2D,err2D]=load_param_madrigal(data_path,[],do_err);
    dt=diff(Time)*86400; name_strategy=sprintf('%.0f',median(dt));
    if std(dt)>10, name_strategy='ant'; end
    return
  end
end

data_path=fullfile(data_path,filesep);
list=getfilelist(data_path,lastfile);
n=length(list);

Time=[]; par2D=[]; par1D=[]; rpar2D=[]; err2D=[];
if n==0, return; end
r_Tsys=[]; r_pp=[]; rres=[]; name_sig=''; name_strategy='';
n_tot=n;
memwarn=0;
lastfile=list(n);
re=6370;
npar2D=9; nrpar2D=3; nerr2D=npar2D-4;
Time=zeros(2,n_tot);
Leap=zeros(2,n_tot);
fileslist=cell2mat({list.file}); fileform='%08d%s'; 
if any(rem(fileslist,1)), fileform='%012.3f%s'; end

for i=1:n_tot
% clear('r_*','name_*')
  load(canon(fullfile(list(i).dir,sprintf(fileform,list(i).file,list(i).ext)),0))
  if exist('r_Offsetppd')
    rOff=r_Offsetppd;
  elseif exist('r_phasepush')
    rOff=r_phasepush;
  else
    rOff=[];
  end
  if isfinite(max_ppw) && exist('r_ppw','var')
    d=find(r_ppw<max_ppw);
    r_pp=r_pp(d); r_pprange=r_pprange(d); r_pperr=r_pperr(d);
  end
  par1=[r_az r_el r_Pt/10000 median(r_Tsys) rOff]; %10kW
  npar1=length(par1);
  if i==1
    npar1D=npar1;
    n_alt=size(r_param,1);
    par2D	=ones(n_alt,n_tot,npar2D)*NaN;
    par1D=zeros(n_tot,npar1D);
    if do_rpar && any(strfind('TVLQ',name_site)) && ~isempty(r_pp)
      n_ralt=length(unique(round(r_pprange/ppres)));
      rpar2D=ones(n_ralt,n_tot,3)*NaN;
    else
      n_ralt=NaN;
    end
    if do_err
      err2D	=ones(n_alt,n_tot,nerr2D)*NaN;
    end
    if isempty(name_ant)
      antennas=['uhf kir sod vhf esr'];
      i=(strfind('TKSVL',name_site)-1)*4+1;
      name_ant=antennas(i:i+2);
    end
    if exist('r_w','var')
      rres=ones(n_tot,1)*Inf;
    end
  end
  nsig=regexprep(name_sig,'(\s[012][0-9]:[0-5][0-9]:[0-5][0-9])$',''); %remove tod
  if exist(name_sig,'var'), nsig=split(name_sig); nsig=char(join(nsig(1:end-1))); end
  if isempty(name_strategy)
    name_strategy=char(regexp(list(i).dir,'[^_]+(?=@)','match')); if strcmp(name_strategy,name_expr), name_strategy=''; end
  end
  if isempty(allnames)
    allnames.ant=name_ant(1:3); allnames.expr=name_expr; allnames.sig=nsig; allnames.strategy=name_strategy;
  else
    if ~contains(row(allnames.ant'),name_ant(1:3)), allnames.ant=char(allnames.ant,name_ant(1:3)); end
    if ~contains(row(allnames.expr'),name_expr), allnames.expr=char(allnames.expr,name_expr); end
    if ~contains(row(allnames.strategy'),name_strategy), allnames.strategy=char(allnames.strategy,name_strategy); end
    if ~contains(row(allnames.sig'),nsig), allnames.sig=char(allnames.sig,nsig); end
  end
  nalt=size(r_param,1);
  if nalt>n_alt
    par2D=[par2D;ones(nalt-n_alt,n_tot,npar2D)*NaN];
    if do_err
      err2D=[err2D;ones(nalt-n_alt,n_tot,nerr2D)*NaN];
    end
    n_alt=nalt;
  end
  n=1:nalt;
  if load_apriori
    r_param=r_apriori;
  else
    d=find(r_status>status(1) | r_res(:,1)>status(2));
    [d1,d2]=find(r_error(d,1:size(r_param,2))>0);
    r_param(d(d1),d2)=NaN;
    r_error(d(d1),d2)=NaN;
  end
  par2D(n,i,[1 2 8 9])=[r_range(:,1) r_h(:,1) r_dp(:,1) r_res(:,1)];
  par2D(n,i,[3 5 7])=r_param(:,myparams);
  par2D(n,i,[4 6])=[r_param(:,2).*r_param(:,3) -r_param(:,5)];
  if do_err
    err2D(n,i,[1 3 5])=r_error(:,myparams);
    err2D(n,i,[2 4])=[(r_error(:,2)./r_param(:,2)+r_error(:,3)./r_param(:,3)).*par2D(n,i,4) r_error(:,5)];
  end
  [Time(:,i),Leap(:,i)]=timeconv(r_time,'utc2mat');
  if npar1>npar1D
   par1D=[par1D zeros(n_tot,npar1-npar1D)];
   npar1D=npar1;
  end 
  par1D(i,1:npar1)=par1;
  if exist('r_w','var') && ~isempty(rres)
    rres(i)=max(r_w(:,3));
  end
  if isfinite(n_ralt)
    [ppr,k,l]=unique(round(r_pprange/ppres)); pp=ppr;
    nalt=length(ppr);
    for n=1:nalt
      d=find(l==n);
      if exist('r_pperr','var')
        w=1../r_pperr(d).^2;
      else
        w=ones(size(d));
      end
      sw=sum(w);
      ppr(n)=sum(r_pprange(d).*w)/sw;
      pp(n)=max(sum(r_pp(d).*w)/sw,1);
    end
    if nalt>n_ralt
      try
        rpar2D=[rpar2D;ones(nalt-n_ralt,n_tot,nrpar2D)*NaN];
        n_ralt=nalt;
      catch
        if strfind(lasterr,'MEMORY')
          if ~memwarn
            warning('GUISDAP:default','Raw density results cut')
          end
          nalt=n_ralt; memwarn=1;
        else
          error(lasterr)
        end
      end
    end
    n=1:nalt;
    rpar2D(n,i,1)=ppr(n);
    x=ppr(n)/re;
    rpar2D(n,i,2)=re*sqrt(1+x.*(x+2*sin(r_el/57.2957795)))-re;
    rpar2D(n,i,3)=pp(n);
  end
end
if ~isempty(rres)
  rres=rres/re;
  rres=re*sqrt(1+rres.*(rres+2*sin(par1D(:,2)/57.2957795)))-re;
  rres=1.1*max(rres)+3*std(rres);
end
% Cast azimuth and elevation into range 0-360, 0-90 degrees
d=find(par1D(:,2)>90); par1D(d,2)=180-par1D(d,2); par1D(d,1)=par1D(d,1)+180;
par1D(:,1)=mod(par1D(:,1)+360,360);

[s1,s]=sort(Time(1,:));
Time=Time(:,s); par2D=par2D(:,s,:); par1D=par1D(s,:);
if ~isempty(rpar2D), rpar2D=rpar2D(:,s,:); end
if ~isempty(err2D), err2D=err2D(:,s,:); end
if isempty(name_strategy)
  dt=diff(Time)*86400; name_strategy=sprintf('%.0f',median(dt));
  if std(dt)>10, name_strategy='ant'; end
  if exist(fullfile(list(1).dir,'.gup'),'file')
    load('-mat',fullfile(data_path,'.gup'),'intper')
    if length(intper)>1
      name_strategy='scan';
    elseif intper>0
      name_strategy=sprintf('%g',intper);
    end
  end
end
